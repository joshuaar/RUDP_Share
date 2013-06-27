package share.app
import share.protocol._
import chat.msg.CONNECT
import chat.msg.CONNECTED
import chat.msg.LISTEN
import chat.msg.msg
import share.protocol.http.httpActor
import share.protocol.http.Dev
import share.protocol.http.REQ_CON
import share.protocol.http.GETPEERS
import share.protocol.http.CON_OUT
import share.protocol.http.CON_IN
import share.protocol.http.ACK_CON
import akka.actor._
import java.net._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object const {
  val uname = "default"
  val trackerHost = "http://smaugshideout.com:3000"
    
  type devMap = Map[String,Dev]
}

class Connections {
  var activeConnections = Map[String,ActorRef]()
  
  def add(devID:String,a:ActorRef) = {
    activeConnections = activeConnections + (devID -> a) 
  }
  
  def pop(devID:String):Option[ActorRef] = {
    val out = activeConnections get devID
    activeConnections = activeConnections - devID
    out
  }
  
  def rm(devID:String) = {
    activeConnections = activeConnections - devID
  }
  def isConnected(devID:String):Boolean = {
    activeConnections get devID match {
      case Some(thing) => true
      case None => false
    }
  }
}

class MainActor extends Actor {
  import context._
  val pendingCon = new Connections()
  val con = new Connections()
  println("*** (MainActor) Initializing HTTP Actor ***")
  val http = context.actorOf(Props(new httpActor(const.uname,const.trackerHost)))
  var knownDevs = Map[String,Dev]()
  
  
  def receive = {
    case devs:const.devMap => {
      knownDevs = devs
      println(s"*** (MainActor) Got peer list $devs ***")
      devs map (x=>{
        if(!con.isConnected(x._1)){
          //Start a connection if its not connected
          val port = wireCodes.getLocalPort()
          val rdev = x._1
          println(s"*** (MainActor) Attempting connection to $rdev on local port $port")
          val cli = context.actorOf(Props(new rudpActor(port)), name = "cli:"+x._1)
          //cli ! LISTEN(x._2.ipExternal,port) //Start listening on some port
          pendingCon.add(x._1,cli)
          http ! REQ_CON(x._2,port) // send the port and connection info to server
        }
      })
    }//Now every device on the list has been checked against existing connections
    //And a connection request has been sent if there is no connection
    
    case CON_OUT(devID,port:Int) => {
      pendingCon.pop(devID) match {
        case Some(actor) => {
          knownDevs get devID match {
            case Some(dev) => {
              val host = dev.ipExternal
              implicit val timeout = Timeout(15 seconds)
              val future = actor ? CONNECT(host,port)
              println("Client actor attempting connection to host")
              future onSuccess {
                case CONNECTED => {
                  println(s"Successfully connected (RUDP) to $host:$port")
                  con.add(devID,actor)
                }
              }
              future onFailure {
                case t:TimeoutException => println(s"(RUDP) connection timed out on $host:$port")
              }

            }
            case None => println("Tried to access an unknown device")
          }
        }
        case None => println("Tried to pop a pending connection that does not exist")
      }
    }
    
    //Recieved an connection request on the wire, meaning we should start a local server
    //to listen for connections from the remote
    case CON_IN(devID,port:Int) => {
      if(!con.isConnected(devID)){

        knownDevs get devID match {
          case Some(dev) =>{
            val lport = wireCodes.getLocalPort()
            val serv = context.actorOf(Props(new rudpActor(lport)),name="serv:"+devID)
            val host = dev.ipExternal
            implicit val timeout = Timeout(15 seconds)
            val future = serv ? LISTEN(host,port,15)
            http ! ACK_CON(dev,lport) // send my local port to the remote device
            future onSuccess {
              case CONNECTED => {
                println(s"Successfully connected (RUDP) to $host:$port")
                con.add(devID,serv)
              }
            }
            future onFailure {
              case t:TimeoutException => println(s"(RUDP) connection timed out on $host:$port")
            }
          }
          case None => println("Tried to access an unknown device")
        }
      }
    }
    
    case s:String => {
      
    }
  }
}

object AppTest extends App {
  val system = ActorSystem("RUDPSystem")
  val main = system.actorOf(Props(new MainActor()), name = "test")
}