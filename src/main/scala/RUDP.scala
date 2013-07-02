package share.protocol
import chat.msg._
//import net.rudp._
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import udt._


object wireCodes {
  val FT_CON = ":c(.*):(.*)".r // Address:Port
  val FT_REQ = "::(.*):(.*):(.*):(.*)".r//resource:IP:Port:offset
  val FT_RDY = ":RDY" //Send this to host when ready for file
  val FT_STOP = ":STP"
  val ECHO_ = ":ECHO:(.*)".r
    
  def sendreq(req:GET,ip:String,port:Int):String ={
    val resource = req.r
    val offset  = req.offset
    return s"::$resource:$ip:$port:$offset"
  }
  
  def getLocalPort():Int = {
    val s= new ReliableServerSocket(0)
    val port = s.getLocalPort()
    s.close()
    return port
  }
  
  def copy(in:InputStream, out:OutputStream):Long = {
        val buf = new Array[Byte](8192)
        var len = 0;
        len = in.read(buf)
        var written:Long = 0
        try{
        while (len != -1) {
            out.write(buf, 0, len)
            written+=len
            //println(s"Sent $len bytes")
            len = in.read(buf)

        }
        }
        catch{
          case s:SocketException => return written
        }
        println("Stream copied")
        return -1
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
  def transferMetaInfo(s:UDTSocket,sz:Long = -1):Long={
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
class rudpSender(s:UDTSocket,parent:ActorRef) extends Actor{
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
    
    case l:LISTEN => {
        
      println(s"Listening on $localPort")
      val rhost = l.host
      val rport = l.port
      puncher.punch(rhost,localPort,rport) //def punch(host:String,localPort:Int,remotePort:Int,retransmits:Int=100)
      Thread.sleep(50)
      UDTReceiver.connectionExpiryDisabled=true
      val serverSocket = new UDTServerSocket(localPort)
      val future = Future{ serverSocket.accept() }
      try{
        
        self ! Await.result(future, l.timeout second).asInstanceOf[UDTSocket] 
        sender ! CONNECTED
        
      }
      catch{case s:TimeoutException => {
        
        println(s"[RUDP]timed out listening for $rhost:$rport")
        
        }
      }
      }
    
    case CONNECT(ip,port) => {
      println(s"Attempting RUDP Connection to $ip on port $port")
      Thread.sleep(100)
      UDTReceiver.connectionExpiryDisabled=true
      val cli = new UDTClient(InetAddress.getLocalHost(),localPort)
      cli.connect(ip,port)
      self ! cli
      sender ! CONNECTED
    }
    
    case s:UDTSocket => {
      //val hostName = s.getInetAddress().getCanonicalHostName()
      //val port = s.getPort()
      listener = context.actorOf(Props(new rudpListener(IOStreams go here,self)), name="listen")
      listener ! LISTEN
      send = new PrintWriter(s.getOutputStream(), true)
      become(connected)
      println(s"connected on remote port $lp")
      }
    case s:UDTClient => {
      blah!
    }
  }
  
  def connected:Receive = {
    case RECVDMESSAGE(s) => {
      println("I got a message on the wire!: "+s)
      //listener ! LISTEN
    }
    case SENDMESSAGE(m) => {
      println("outer sending message")
      send.println(m)
      println("outer sent")
    }
    
    case ECHO(m) => {
      send.println(s":ECHO:$m")
    }
    
    case req:GET => {
      //GET(r:String,destination:String,offset:Long=0)
      val resource = req.r
      val destination = req.destination
      //Determine from STUN server which IP and port to connect on
      val localPort = wireCodes.getLocalPort()
      val lh = localHost.getHostName() 
      
      //Initialize listener
      val fileGetter = context.actorOf(Props(new fileListener(localPort,req)))
      
      send.println(wireCodes.sendreq(req,lh,localPort)) // send a request for a resource

    }
    
    case s:SEND => { //got a request from remote for resource
      val lPort = wireCodes.getLocalPort()
      //Hole punch code goes here
      //
      println("Got request from remote for a resource")
      val fileSender = context.actorOf(Props(new fileSender(lPort,s,localHost)))
    }
  }
}

class fileSender(lPort:Int,send:SEND,localHost:InetAddress) extends Actor{
  val sock = new UDTClient(localHost,lPort)
  sock.connect(send.host,send.port)
  println("file sender is connected")
  self ! sock
  
  //WRITE FILES TO SOCKETS
  def receive = {
    case s:UDTClient => {
      val ft = new FileTransfer(s)
      ft.send(send.resource,send.offset)
      self ! PoisonPill
    }
  }
}

class fileListener(lp:Int,req:GET) extends Actor{
  import context._
  val destination = req.destination
  //ServerSocket attempt a listen
  val serverSocket = new ReliableServerSocket(lp)
  val future = Future{ serverSocket.accept() }
  val sock:ReliableSocket = null
  println("File receiver beginning connection")
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
      println("File Receiver is connected")
      val ft = new FileTransfer(s)
      val newOff = ft.get(destination,"",req.offset)
      if(newOff != -1)
    	  parent ! GET(req.r,destination,newOff)
      self ! PoisonPill
    }
    case _ => println("Received a message")
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
        case wireCodes.FT_REQ(id,host,port,offset) => {
          val f = new File(id)
          parent ! SEND(id,host,port.toInt,offset.toLong) //Got a request for a resource
          println(s"Recieved a get request from $host, sending to handler")
        }
        case wireCodes.FT_STOP => {
          println("stopped listening on request from remote")
        }
        case wireCodes.FT_CON(ip,port) => {
          println("Now I got the connection information from remote")
          parent ! (ip,port)
        }
        case wireCodes.ECHO_(m) => {
          parent ! SENDMESSAGE(m)
        }
        case s:String => {
          parent ! RECVDMESSAGE(s)
          println("Sent unknown message to handler")
        }
      }
      self ! LISTEN
      println("Listener sent reactivation signal")
    }
  }//End cmdListener
}

object api {
  val system = ActorSystem("RUDPSystem")
  def makeServer(name:String,port:Int,host:String,remotePort:Int):ActorRef = {
	  val serv = system.actorOf(Props(new rudpActor(port)), name = name)
	  serv ! LISTEN(host,remotePort,10)//LISTEN(host:String,port:Int,timeout:Int=0)
	  return serv
  }
  def makeClient(name:String,port:Int,host:String,remotePort:Int):ActorRef = {
    val cli = system.actorOf(Props(new rudpActor(port)), name = name)
    cli ! CONNECT(host,remotePort)
    return cli
  }
}

object rudp extends App {

  val system = ActorSystem("ChatSystem")
  
  if(args(3).toInt == 1){
    val serv = api.makeServer("serv",args(1).toInt,args(0),args(2).toInt)
  }
  else{
    val serv = api.makeClient("cli",args(1).toInt,args(0),args(2).toInt)
    Thread.sleep(500)
    serv ! ECHO("ECHOTEST!!")
  }
  
  //val serv = api.makeServer("serv",41843,"localhost",46170)
  //val cli = api.makeClient("cli",46170,"localhost",41843)
  //serv ! LISTEN
  //Thread.sleep(500)
  //println("Attempting connection")
  //cli ! CONNECT("localhost",6004)
 // println("Testing Message sending")
  //Thread.sleep(1000)
  
  //Test send message
  //cli ! ECHO("ECHOME")
  //serv ! SENDMESSAGE("Message2")
  
  //Test file request
  //cli ! GET("/home/josh/test","/home/josh/testCopy")
  //cli ! GET("/home/josh/test2","/home/josh/testCopy")
  //serv ! GET("/home/josh/CIM/Research/labdata/jaricher/newDecipher/Data for Database/Array Results/First Chip Disease Dataset/llnl.csv","/home/josh/test3")
  
}