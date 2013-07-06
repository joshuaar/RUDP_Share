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
import udt._

case class Con(serv:Server,cli:Client)

  
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
  
  implicit def g2g(g:GET) = getReq(g.r,g.offset)
  implicit def s2g(s:SEND) = getReq(s.resource,s.offset)
  
  val localPort = lp
  val localHost = InetAddress.getLocalHost()
  var listener:ActorRef = null
  var cli:Client = null
  var serv:Server = null
  import context._
  def receive = {
    
    case l:LISTEN => {
        
      println(s"Listening on $localPort")
      val rhost = l.host
      val rport = l.port
      try{
        val s = new Server(localPort,rhost,rport)
        val c = new Client(localPort-1,rhost,rport-1)
        self ! Con(s,c)
        sender ! CONNECTED
      }
      catch{case s:TimeoutException => {
        
        println(s"[RUDP]timed out listening for $rhost:$rport")
        
        }
      }
      }
    
    case CONNECT(ip,port) => {
      println(s"Attempting RUDP Connection to $ip on port $port")
      try{
        val c = new Client(localPort,ip,port)
        val s = new Server(localPort-1,ip,port-1)
        self ! Con(s,c)
        sender ! CONNECTED
      }
      catch{case s:TimeoutException => {
        println(s"[RUDP]timed out listening for $ip:$port")
        }
      }
    }
    
    case Con(server,client) => {
      
      listener = context.actorOf(Props(new rudpListener(server)), name="listen")
      listener ! LISTEN
      cli = client
      serv = server
      become(connected)
      println(s"connected on remote port $lp")
      }
  }
  
  def connected:Receive = {
    
    case a:any => {
      Client.writeString(a.toString(), cli.os)
    }
    
    case req:GET => {
      //GET(r:String,destination:String,offset:Long=0)
      val resource = req.r
      val destination = req.destination
      //Determine from STUN server which IP and port to connect on
      val res = cli.getFile(req,destination)
      if(res != -1){
        val newOffset = res+req.offset
        self ! GET(req.r,req.destination,newOffset)
      }
      else {
        println(s"File $resource gotten successfully")
      }
    }
  }
}



//Listens for messages and files from remote
class rudpListener(s:Server) extends Actor{
  import context._
  def receive = {
    //LISTEN FOR COMMANDS
    //	Program new behaviors here, translate from wire codes to actor instructions
    case LISTEN => {
      println("Listener Online")
      try{
      val data = s.listen()
      data match {
        case g:getReq => {
          println(s"Received a get request, handling")
          s.sendFile(g)
        }
        case a:any => {
          val matcher = "GeT:(.*):(.*)".r
          a.msg match {
            case getReq.matcher(a,b) => println("matched getreq")
            case _ =>
          }
          println("Received an unknown request: "+a.msg)
        }
      }
      self ! LISTEN
      println("Listener sent reactivation signal")
      }
      catch {
        case e:TimeoutException => {
          self ! LISTEN
          println("Listener sent reactivation signal")
        }
      }
    }
  }//End cmdListener
}

object api {
  val system = ActorSystem("RUDPSystem")
  def makeServer(name:String,port:Int,host:String,remotePort:Int):ActorRef = {
    println("Making Server")
	  val serv = system.actorOf(Props(new rudpActor(port)), name = name)
	  serv ! LISTEN(host,remotePort,10)//LISTEN(host:String,port:Int,timeout:Int=0)
	  return serv
  }
  def makeClient(name:String,port:Int,host:String,remotePort:Int):ActorRef = {
    println("Making Client")
    val cli = system.actorOf(Props(new rudpActor(port)), name = name)
    cli ! CONNECT(host,remotePort)
    return cli
  }
}

object rudp{// extends App {

  val system = ActorSystem("ChatSystem")
  
  //if(args(3).toInt == 1){
    //val serv = api.makeServer("serv",args(1).toInt,args(0),args(2).toInt)
  //}
  //else{
    //val serv = api.makeClient("cli",args(1).toInt,args(0),args(2).toInt)
    //Thread.sleep(500)
    //serv ! ECHO("ECHOTEST!!")
  //}
  
  val serv = api.makeServer("serv",41843,"localhost",46170)
  val cli = api.makeClient("cli",46170,"localhost",41843)
  Thread.sleep(3000)
  println("Sending message")
  //while(true){
  cli ! any(readLine)
  Thread.sleep(500)
  cli ! GET("/home/josh/test","/home/josh/testCopy",0)
  //}
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