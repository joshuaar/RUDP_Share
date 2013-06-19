import chat.msg._
import net.rudp._
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._


object wireCodes {
  val FT_CON = ":c(.*):(.*)".r
  val FT_REQ = "::(.*):(.*):(.*):(.*)".r//Send this plus to request a resource from server
  val FT_RDY = ":RDY" //Send this to host when ready for file
  val FT_STOP = ":STP"
  def sendreq(resource:String,ip:String,port:Int,position:Long):String ={
    return s"::$resource:$ip:$port:$position"
  }
  
  def getLocalPort():Int = {
    val s= new ReliableServerSocket(0)
    val port = s.getLocalPort()
    s.close()
    return port
  }
  
  def copy(in:InputStream, out:OutputStream):Int = {
        val buf = new Array[Byte](8192)
        var len = 0;
        len = in.read(buf)
        var written = 0
        while (len != -1) {
            out.write(buf, 0, len)
            written+=len
            //println(s"Sent $len bytes")
            try{
              if(written > 10){//ARREST TRANSFER FOR TESTING PURPOSES
                println("[copy]ARRESTING TRANSFER!")
                return written
              }
              len = in.read(buf)
            }
            catch{
              case s:SocketException => return written
            }
        }
        return written
    }
  def resumeCopy(in:RandomAccessFile, out:OutputStream):Int = {
        val buf = new Array[Byte](8192)
        var len = 0;
        len = in.read(buf)
        var written = 0
        while (len != -1) {
            out.write(buf, 0, len)
            written+=len
            //println(s"Sent $len bytes")
            try{
              len = in.read(buf)
            }
            catch{
              case s:SocketException => return written
            }
        }
        return written
    }
  
  /**
   * sends/receives size of file ahead of file transfer
   * send = True => send the meta info
   * send = False => receive meta info
   */
  def transferMetaInfo(s:ReliableSocket,sz:Long = -1):Long={
    println(s"[transferMetaInfo]: $sz")
    val in = new BufferedReader(new InputStreamReader(s.getInputStream()))
    val out = new PrintWriter(s.getOutputStream(), true)
    val z = sz == -1
    println(s"[transferMetaInfo]:Size is -1 ? $z")
    if(sz != -1){
      if(in.readLine() != "::") {
        throw new SocketException("Unexpected signal from remote")
      }
      out.println(sz)
      return 0
    }
    out.println("::")
    val size = in.readLine()
    println(s"[transferMetaInfo]Got size $size")
    return size.toLong
  }
  
}

//Sends messages and files to remote
class rudpSender(s:ReliableSocket,parent:ActorRef) extends Actor{
  import context._
  var out = new PrintWriter(s.getOutputStream(), true);
  def receive = {
    case SENDMESSAGE(m) => {
      println("[rudpSender]inner send message")
      out.println(m)
      println("[rudpSender]inner sent")
    }
  }
}

//Listens for messages and files from remote
class rudpListener(s:ReliableSocket,parent:ActorRef) extends Actor{
  import context._
  var in = new BufferedReader(new InputStreamReader(s.getInputStream()))
  def receive = {
    case LISTEN => {
      become(cmdListen)
      self ! LISTEN
    }
  }
  def cmdListen:Receive = {
    //LISTEN FOR COMMANDS
    case LISTEN => {
      println("[rudpListener]Listener Online")
      val data = in.readLine()
      data match {
        //Got a GET request from remote
        case wireCodes.FT_REQ(id,host,port,position) => {
          val f = new File(id)
          parent ! SEND(id,host,port.toInt,f.length,position.toLong) //Got a request for a resource
          println(s"[rudpListener]Recieved a get request from $host, sending to handler")
        }
        case wireCodes.FT_STOP => {
          println("[rudpListener]stopped listening on request from remote")
        }
        case wireCodes.FT_CON(ip,port) => {
          println("[rudpListener]Now I got the connection information from remote")
          parent ! (ip,port)
        }
        case s:String => {
          parent ! RECVDMESSAGE(s)
          println("[rudpListener]Sent unknown message to handler")
        }
      }
      self ! LISTEN //Reopen listener
    }
  }//End cmdListener
}

class fileListener(lp:Int,resource:String,destination:String,position:Long) extends Actor{
  import context._
  
  //ServerSocket attempt a listen
  val serverSocket = new ReliableServerSocket(lp)
  val future = Future{ serverSocket.accept() }
  val sock:ReliableSocket = null
  println("[fileListener]File getter beginning connection")
  //Failure to connect, or connect
  try{ self ! Await.result(future, 60 second).asInstanceOf[ReliableSocket] }
  catch{ 
    case t:TimeoutException => {
      println("[fileListener]File listen timeout")
      self ! PoisonPill // kill
      }
    }
  //READ FILES FROM SOCKETS
  def receive = {
    case s:ReliableSocket => {
      println("[fileListener]File getter is connected")
      Thread.sleep(500)
      val size = wireCodes.transferMetaInfo(s)
      val buffer = new Array[Byte](1024)
      val f = new File(destination)
      var append = false
      if(position != 0){
        append = true
      }
      val fileOut = new FileOutputStream(f,append)
      val fileIn = s.getInputStream()
      println("[fileListener]Started file receiving")
      val written = wireCodes.copy(fileIn,fileOut)
      println(s"[fileListener]written: $written, size: $size")
      if(written != size){
        s.close()
        println("[fileListener]Transfer not fully completed, attempting restart")
        context.parent ! GET(resource,destination,written)
        self ! PoisonPill
      }
      else {
      s.close()
      fileOut.close()
      println("[fileListener]Finished file reception")
      self ! PoisonPill
      }
    }
    case _ => println("[fileListener]Received a message")
  }
}

class fileSender(filePath:String,host:String,port:Int,localHost:InetAddress,localPort:Int,size:Long,position:Long) extends Actor{
  val sock = new ReliableSocket(host,port,localHost,localPort)
  println("[fileSender]file sender is connected")
  self ! sock
  
  //WRITE FILES TO SOCKETS
  def receive = {
    case s:ReliableSocket => {
      Thread.sleep(500)
      wireCodes.transferMetaInfo(s, size-position)
      val buffer = new Array[Byte](1024)
      var f = new File(filePath)
      val fileOut = s.getOutputStream()
      if(position != 0){// file not from beginning (in case of resumes)
        val raf = new RandomAccessFile(f,"rw")
        raf.seek(position)//first unsent byte
        wireCodes.resumeCopy(raf,fileOut)
      }
      else{
    	  val fileIn = new FileInputStream(f)
    	  wireCodes.copy(fileIn,fileOut)
    	  fileOut.close()
    	  fileIn.close() // close file
      }
      s.close()
      //Now file has been sent
      println("[fileSender]Finished file sending")
      self ! PoisonPill
    }
  }
}

/**
* Starts listening on a given port
*/
class rudpActor(lp:Int) extends Actor{
  val localPort = lp
  val localHost = InetAddress.getLocalHost()
  var listener:ActorRef = null
  var send:PrintWriter = null
  
  import context._
  def receive = {
    
    case LISTEN => {
      println(s"[rudpActor]Listening on $localPort")
      val serverSocket = new ReliableServerSocket(localPort)
      self ! serverSocket.accept()
      }
    
    case CONNECT(ip,port) => {
      println(s"[rudpActor]Attempting RUDP Connection to $ip on port $port")
      val cli = new ReliableSocket(ip,port,localHost,localPort)
      self ! cli
    }
    
    case s:ReliableSocket => {
      val hostName = s.getInetAddress().getCanonicalHostName()
      val port = s.getPort()
      listener = context.actorOf(Props(new rudpListener(s,self)), name="listen")
      listener ! LISTEN
      send = new PrintWriter(s.getOutputStream(), true)
      become(connected)
      println(s"[rudpActor]connected to $hostName on remote port $port")

      }
  }
  
  def connected:Receive = {
    case RECVDMESSAGE(s) => {
      println("[rudpActor]I got a message on the wire!: "+s)
      //listener ! LISTEN
    }
    case SENDMESSAGE(m) => {
      println("[rudpActor]outer sending message")
      send.println(m)
      println("[rudpActor]outer sent")
    }
    //Self requests a resource
    case GET(resource,destination,position) => {
      println("[rudpActor]:Received Get request")
      //Determine from STUN server which IP and port to connect on
      val localPort = wireCodes.getLocalPort()
      val lh = localHost.getHostName() 
      
      //Initialize listener
      println("[rudpActor]:Initializing file listener")
      val fileGetter = context.actorOf(Props(new fileListener(localPort,resource,destination,position)))
      println("[rudpActor]:asking remote to connect to listener")
      send.println(wireCodes.sendreq(resource,lh,localPort,position)) // send a request for a resource

    }
    
    //Self needs to send a resource
    case SEND(resource,host,port,size,position) => { //got a request from remote for resource
      println("[rudpActor]:Received Send request")
      val lPort = wireCodes.getLocalPort()
      //Hole punch code goes here
      //
      println("[rudpActor]Got request from remote for a resource")
      val fileSender = context.actorOf(Props(new fileSender(resource,host,port,localHost,lPort,size,position)))
    }
  }
}

object rudp extends App {
  
  val system = ActorSystem("ChatSystem")
  val serv = system.actorOf(Props(new rudpActor(6004)), name = "serv")
  val cli = system.actorOf(Props(new rudpActor(6005)), name = "cli")
  serv ! LISTEN
  Thread.sleep(500)
  println("App:Attempting connection")
  cli ! CONNECT("localhost",6004)
  println("App:Testing Message sending")
  Thread.sleep(1000)
  
  //Test send message
  cli ! SENDMESSAGE("Message1")
  serv ! SENDMESSAGE("Message2")
  
  //Test file request
  cli ! GET("/home/josh/test","/home/josh/testCopy")
  //serv ! GET("/home/josh/CIM/Research/labdata/jaricher/newDecipher/Data for Database/Array Results/First Chip Disease Dataset/llnl.csv","/home/josh/test3")
  
}