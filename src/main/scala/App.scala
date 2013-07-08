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
import share.protocol.http.Shared
import share.protocol.http.httpFuncs
import akka.actor._
import java.net._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import akka.actor.AllForOneStrategy
import akka.actor.SupervisorStrategy._

case class BROADCAST(msg:String)

object const {
  val uname = "default"
  val trackerHost = "http://smaugshideout.com:3000"
    
  type devMap = Map[String,Dev]
  
  def getDevInfo(devID:String):Option[Dev] = {
    httpFuncs.getDevInfo(devID,s"$trackerHost/u/$uname")
  }
  
}

class Connections {
  var activeConnections = Map[String,ActorRef]()
  
  def getAll():List[ActorRef] = {
    return activeConnections.values.toList
  }
  
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
  
  override val supervisorStrategy = 
     AllForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
     case _:ConnectException => {//The server is probably down. Keep trying (forever?)
       println("Connection Refused, retrying")
       Thread.sleep(10000)
     }
     Restart
    case _:Exception => Escalate
}
  
  val pendingCon = new Connections()
  val con = new Connections()
  println("*** (MainActor) Initializing HTTP Actor ***")
  val http = context.actorOf(Props(new httpActor(const.uname,const.trackerHost)),name="httpCom")
  var knownDevs = Map[String,Dev]()
  
  def decideIP(remoteDev:Dev):String = {
    val myDev = Shared.getDev()
    val host = remoteDev.ipExternal
    if(myDev.ipExternal == host){//Local and remote share the same external ip, so we must connect via local net
      return remoteDev.ipLocal
      }
    return host
  }
  
  
  
  def receive = {
    /**
     * Received a list of remote devices from http
     * This happens when first connecting, or when the connection is refreshed.
     * This handler figures out which devices need to be connected, 
     * adds those to pending connections, and sends those requests off to http
     */
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
    
    /**
     * Comes from http when an acknowledgement has been received from a remote host.
     * When this message is recieved, the remote host should have already punched a hole
     * and is waiting for an inbound connection.
     */
    case CON_OUT(devID,port:Int) => {
      println(s"Received connection confirmation (CON_OUT) from remote DevID:$devID")
      pendingCon.pop(devID) match {
        case Some(actor) => {
          knownDevs get devID match {
            case Some(dev) => {
              //Decide which IP to use (internal or external)
              val host = decideIP(dev)
              
              implicit val timeout = Timeout(60 seconds)
              val future = actor ? CONNECT(host,port)
              println("Client actor attempting connection to host")
              future onSuccess {
                case CONNECTED => {
                  println(s"Successfully connected (RUDP) to $host:$port")
                  con.add(devID,actor)
                }
              }
              future onFailure {
                case t:TimeoutException => {
                  println(s"(RUDP) client connection timed out on $host:$port")
                  actor ! PoisonPill
                }
              }

            }
            case None => {
              println("Device not known locally, asking server (CON_OUT)")
             const.getDevInfo(devID) match {
                case Some(dev) => {
                  knownDevs += (devID->dev)
                  pendingCon.add(devID,actor) //reset old state
                  self ! CON_OUT(devID,port)
                  
                }
                case None => println("Tried to access an unknown device, request ignored")
              }
            }
          }
        }
        case None => println("Tried to pop a pending connection that does not exist")
      }
    }
    /**
     * Recieved an connection request on the wire, meaning we should start a local server
     * to listen for connections from the remote. We also need to figure out which IP
     * the remote should attempt a connection on (external or internal)
     * devID: the remote's device ID
     * port: the remote's port
     */
    case CON_IN(devID,port:Int) => {
      println(s"Received connection request (CON_IN) from DevID: $devID")
      if(!con.isConnected(devID)){

        knownDevs get devID match {
          case Some(dev) =>{
            val lport = wireCodes.getLocalPort()
            val serv = context.actorOf(Props(new rudpActor(lport)),name="serv:"+devID)
            //Decide which IP to connect on
            val host = decideIP(dev)
            
            implicit val timeout = Timeout(60 seconds)
            val future = serv ? LISTEN(host,port,60)
            http ! ACK_CON(dev,lport) // send my local port to the remote device
            future onSuccess {
              case CONNECTED => {
                println(s"Successfully connected (RUDP) to $host:$port")
                con.add(devID,serv)
              }
            }
            future onFailure {
              case t:TimeoutException => {
                println(s"(RUDP) server connection timed out on $host:$port")
                serv ! PoisonPill
                }
            }
          }
          case None => {
            println("Device not known locally, asking server (CON_IN)")
            const.getDevInfo(devID) match {
              case Some(dev) => {
                val remoteDevID = dev.devID
                println(s"Found DevID: $remoteDevID on the server")
                knownDevs += (devID->dev)
                self ! CON_IN(devID,port)
              }
              case None => println("Tried to access an unknown device, request ignored")
            }
          }
        }
      }
    }
        
    case s:String => {
      
    }
  }
}

object AppTest extends App {
  System.setProperty("java.net.preferIPv4Stack" , "true")
  val system = ActorSystem("RUDPSystem")
  val main = system.actorOf(Props(new MainActor()), name = "test")
}