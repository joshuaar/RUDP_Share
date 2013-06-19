package share.protocol
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
  val FT_CON = ":c(.*):(.*)".r // Address:Port
  val FT_REQ = "::(.*):(.*):(.*)".r//resource:IP:Port
  val FT_RDY = ":RDY" //Send this to host when ready for file
  val FT_STOP = ":STP"
  def sendreq(resource:String,ip:String,port:Int):String ={
    return s"::$resource:$ip:$port"
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
            len = in.read(buf)

        }
        return written
    }
  
  /**
   * sends/receives size of file ahead of file transfer
   * size = -1 => receive meta info from socket
   * size /= -1 => send meta info located in variable size
   * 
   * Usage client:
   * 	val fromServer = transferMetaInfo(s) // fromServer == 21305
   * 
   * Usage server:
   * 	transferMetaInfo(s,21305) // Size of file is 21305 bytes
   */
  def transferMetaInfo(s:ReliableSocket,sz:Long = -1):Long={
    println(sz)
    val in = new BufferedReader(new InputStreamReader(s.getInputStream()))
    val out = new PrintWriter(s.getOutputStream(), true)
    println(sz == -1)
    if(sz != -1){
      if(in.readLine() != "::") {
        throw new SocketException("Unexpected signal from remote")
      }
      out.println(sz)
      return 0
    }
    out.println("::")
    val size = in.readLine()
    println(s"Got size $size")
    return size.toLong
  }
  
}

//Sends messages and files to remote
class rudpSender(s:ReliableSocket,parent:ActorRef) extends Actor{
  import context._
  var out = new PrintWriter(s.getOutputStream(), true);
  def receive = {
    case SENDMESSAGE(m) => {
      println("inner send message")
      out.println(m)
      println("inner sent")
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
    //	Program new behaviors here, translate from wire codes to actor instructions
    case LISTEN => {
      println("Listener Online")
      val data = in.readLine()
      data match {
        case wireCodes.FT_REQ(id,host,port) => {
          val f = new File(id)
          parent ! SEND(id,host,port.toInt,f.length) //Got a request for a resource
          println(s"Recieved a get request from $host, sending to handler")
        }
        case wireCodes.FT_STOP => {
          println("stopped listening on request from remote")
        }
        case wireCodes.FT_CON(ip,port) => {
          println("Now I got the connection information from remote")
          parent ! (ip,port)
        }
        case s:String => {
          parent ! RECVDMESSAGE(s)
          println("Sent unknown message to handler")
        }
      }
    }
  }//End cmdListener
}

class fileListener(lp:Int,destination:String) extends Actor{
  import context._
  
  //ServerSocket attempt a listen
  val serverSocket = new ReliableServerSocket(lp)
  val future = Future{ serverSocket.accept() }
  val sock:ReliableSocket = null
  println("File getter beginning connection")
  //Failure to connect, or connect
  try{ self ! Await.result(future, 10 second).asInstanceOf[ReliableSocket] }
  catch{ 
    case t:TimeoutException => {
      println("File listen timeout")
      self ! PoisonPill // kill
      }
    }
  //READ FILES FROM SOCKETS
  def receive = {
    case s:ReliableSocket => {
      println("File getter is connected")
      Thread.sleep(500)
      val size = wireCodes.transferMetaInfo(s)
      val buffer = new Array[Byte](100)
      val f = new File(destination)
      val fileOut = new FileOutputStream(f)
      val fileIn = s.getInputStream()
      println("Started file receiving")
      val written = wireCodes.copy(fileIn,fileOut)
      
      if(written != size){
        println("Transfer not fully completed")
      }
      s.close()
      fileOut.close()
      println("Finished file reception")
      self ! PoisonPill
    }
    case _ => println("Received a message")
  }
}

class fileSender(filePath:String,host:String,port:Int,localHost:InetAddress,localPort:Int,size:Long) extends Actor{
  val sock = new ReliableSocket(host,port,localHost,localPort)
  println("file sender is connected")
  self ! sock
  
  //WRITE FILES TO SOCKETS
  def receive = {
    case s:ReliableSocket => {
      Thread.sleep(500)
      wireCodes.transferMetaInfo(s, size)
      val buffer = new Array[Byte](2048)
      val f = new File(filePath)
      val fileIn = new FileInputStream(f)
      val fileOut = s.getOutputStream()
      wireCodes.copy(fileIn,fileOut)
      fileOut.close()
      fileIn.close() // close file
      s.close()
      //Now file has been sent
      println("Finished file sending")
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
      println(s"Listening on $localPort")
      val serverSocket = new ReliableServerSocket(localPort)
      self ! serverSocket.accept()
      }
    
    case CONNECT(ip,port) => {
      println(s"Attempting RUDP Connection to $ip on port $port")
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
      println(s"connected to $hostName on remote port $port")

      }
  }
  
  def connected:Receive = {
    case RECVDMESSAGE(s) => {
      println("I got a message on the wire!: "+s)
      listener ! LISTEN
    }
    case SENDMESSAGE(m) => {
      println("outer sending message")
      send.println(m)
      println("outer sent")
    }
    case GET(resource,destination) => {
      
      //Determine from STUN server which IP and port to connect on
      val localPort = wireCodes.getLocalPort()
      val lh = localHost.getHostName() 
      
      //Initialize listener
      val fileGetter = context.actorOf(Props(new fileListener(localPort,destination)))
      
      send.println(wireCodes.sendreq(resource,lh,localPort)) // send a request for a resource

    }
    
    case SEND(resource,host,port,size) => { //got a request from remote for resource
      val lPort = wireCodes.getLocalPort()
      //Hole punch code goes here
      //
      println("Got request from remote for a resource")
      val fileSender = context.actorOf(Props(new fileSender(resource,host,port,localHost,lPort,size)))
    }
  }
}

object api {
  val system = ActorSystem("RUDPSystem")
  def makeServer(name:String,port:Int):ActorRef = {
	  val serv = system.actorOf(Props(new rudpActor(port)), name = name)
	  serv ! LISTEN
	  return serv
  }
  def makeClient(name:String,port:Int,host:String,remotePort:Int):ActorRef = {
    val cli = system.actorOf(Props(new rudpActor(port)), name = name)
    cli ! CONNECT(host,remotePort)
    return cli
  }
}

object rudp extends App {
  
  //val system = ActorSystem("ChatSystem")
  val serv = api.makeServer("serv",6004)
  val cli = api.makeClient("cli",6005,"localhost",6004)
 // serv ! LISTEN
  Thread.sleep(500)
  println("Attempting connection")
 // cli ! CONNECT("localhost",6004)
 // println("Testing Message sending")
  Thread.sleep(1000)
  
  //Test send message
  cli ! SENDMESSAGE("Message1")
  serv ! SENDMESSAGE("Message2")
  
  //Test file request
  cli ! GET("/home/josh/test","/home/josh/testCopy")
  //serv ! GET("/home/josh/CIM/Research/labdata/jaricher/newDecipher/Data for Database/Array Results/First Chip Disease Dataset/llnl.csv","/home/josh/test3")
  
}