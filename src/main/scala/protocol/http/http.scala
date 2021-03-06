package share.protocol.http
import akka.actor._
import java.net._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.AllForOneStrategy
import akka.actor.SupervisorStrategy._
import java.net.ConnectException

import uk.co.bigbeeconsultants.http._
import uk.co.bigbeeconsultants.http.response.Response
import uk.co.bigbeeconsultants.http.request.RequestBody
import java.net.URL
import scala.util.parsing.json._
import scala.concurrent.stm._

object Shared {
  val thisDev:Ref[Option[Dev]] = Ref(None)
  def getDev():Dev = {
    atomic { implicit txn =>
      thisDev() match {
        case Some(d) => return d
        case None => throw new DevException("Device not registered")
      }
    }
  }
  def setDev(d:Dev) = {
    atomic { implicit txn =>
      thisDev() = Option(d)
    }
  }
  def getDevID():String ={
    atomic {implicit txn =>
      return getDev.devID
    }
  }
}

object Dev {
  def fromMap(m:Map[String,String]):Dev = {
    new Dev(m getOrElse ("ipLocal","nil"), 
          m getOrElse("ipExternal","nil"), 
          m getOrElse("devID","nil"))
  }
  def fromJSON(j:String):Dev = {
    val js = JSON.parseFull(j) match {
      case Some(stuff)=>stuff.asInstanceOf[Map[String,String]]
      case None => throw new JSONException("JSON parse error while creating device")
    }
    return Dev.fromMap(js)
  }
}
case class Dev(ipLocal:String,ipExternal:String,devID:String)
case class JSONException(msg:String) extends Exception
case class ConnException(msg:String) extends Exception
case class DevException(msg:String) extends Exception
case class ForbiddenException(msg:String) extends Exception
case class DevNotFoundException(msg:String) extends Exception
abstract class msg
case class LISTEN(dev:String) extends msg
case class SEND(s:String,dev:String) extends msg
case class DEVID(id:String) extends msg
case class GETPEERS extends msg
case class REQ_CON(d:Dev,port:Int)
case class CON_OUT(devID:String,port:Int)
case class CON_IN(devID:String,port:Int)
case class ACK_CON(devID:Dev,port:Int)

//former wire codes
case class ASK_CON(devID:String,port:String) 
object ASK_CON {
    def fromMap(m:Map[String,String]):ASK_CON = {
    new ASK_CON(m getOrElse ("devID","nil"), 
          m getOrElse("port","nil"))
  }
  def fromJSON(j:String):ASK_CON = {
    val js = JSON.parseFull(j) match {
      case Some(stuff)=>stuff.asInstanceOf[Map[String,String]]
      case None => throw new JSONException("JSON parse error while parsing ask request")
    }
    fromMap(js)
  }
}

case class ACK(devID:String,port:String)
object ACK {
    def fromMap(m:Map[String,String]):ACK = {
    new ACK(m getOrElse ("devID","nil"), 
          m getOrElse("port","nil"))
  }
  def fromJSON(j:String):ACK = {
    val js = JSON.parseFull(j) match {
      case Some(stuff)=>stuff.asInstanceOf[Map[String,String]]
      case None => throw new JSONException("JSON parse error while parsing ack request")
    }
    fromMap(js)
  }
}

object httpFuncs {
  val config = Config(connectTimeout = 10000, readTimeout = 60000)
  
  def getDevInfo(devID:String,prefix:String):Option[Dev] = {
    def http = new HttpClient(config)
    val url = s"$prefix/$devID"
    val res = http.get(new URL(url))
    println(s"---[httpFuncs] Device info for $devID:")
    println(res.body.asString)
    try{
    	return Some(Dev.fromJSON(res.body.asString))
    }
    catch {
      case e:JSONException => return Option.empty
    }
  }
}

class httpActor(uid:String, trackerHost:String) extends Actor {
  import context._
  
  //Supervisor Strategy
  override val supervisorStrategy = 
     AllForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 2 minute) {
    case _:ForbiddenException => {
      Thread.sleep(10000)
      Restart
      }
    case _:DevNotFoundException => {
      Restart
    }
//    case _:ConnectException => {//The server is probably down. Keep trying (forever?)
//      Thread.sleep(10000)
//      Restart
//    }
    case _:Exception => Escalate
    }
  
  println("***(httpActor) Initializing HTTP communications***")
  
  var devID = ""
    
  def http = new HttpClient(httpFuncs.config)
  val listener = context.actorOf(Props(new httpListener(uid,trackerHost)))
  
  //context.parent ! getPeers // send initial peers list to main actor
  
  //wire codes

  
  /**
   * This gets picked up by the ASK_CON handler
   */
  def reqConnect(d:Dev,port:Int) = {
    val dID = d.devID
    println(s"Asking remote device $dID to connnect on local port $port")
    val res = sendMap(d.devID,Map[String,String](("type","ask"),("devID",devID),("port",port.toString)))

  }
  
  def sendAck(d:Dev,port:Int) = {
    if(devID==""){
      throw new Exception("We don't have a local device id!")
    }
    val dID = d.devID
    println(s"sending Ack to remote device $dID and local port $port")
    val res = sendMap(d.devID,Map[String,String](("type","ack"),("devID",devID),("port",port.toString)))
  }
    
  def getPeers():Map[String,Dev] = {
    val url = s"$trackerHost/u/$uid"
    val res = http.get(new URL(url))
    val js = JSON.parseFull(res.body.asString)
    val devList = js match {
    case Some(json) => {
      //Holy shit this is harder than it needs to be
      val connectableDevs = json.asInstanceOf[List[Map[String,String]]].map(
          i => Dev.fromMap(i))
      connectableDevs
    }
    case None => {
      val connectableDevs = List[Dev]()
      throw new JSONException("JSON parse failed. Is server sending properly formatted JSON?")
      connectableDevs
    }
  }
    
    ( devList map {i => (i.devID,i)} toMap ) - devID //All peers but self
  }
  
  def sendMsg(dev:String,msg:String):String = {
    return sendMap(dev,Map("type"->"msg","msg"->msg))
  }
  
  def sendMap(dev:String,map:Map[String,String]):String = {
    println("Sending Map "+map.toString)
    def http = new HttpClient(httpFuncs.config)
    val url = s"$trackerHost/p/$dev"
    val req = RequestBody(map)
    val res = http.post(url, Some(req))
    return res.body.asString
  }
  
  def receive = {
    case SEND(msg,dev) => {
      //val future = Future{ sendMsg(dev,msg) }
      sendMsg(dev,msg)
      
    }
    
    /**
     * This comes from the main actor.
     * It is trying to ask a remote host to connect.
     */
    case REQ_CON(d:Dev,port:Int) => {
      if(devID!=""){
        println(s"***(httpActor) Sending a request for connection on $d:$port***")
    	  reqConnect(d,port)
      }
      else{
        println("Cannot request connections, no DevID found")
      }
    }
    
    case GETPEERS => {
      sender ! getPeers()
    }
    
    //We just got our device ID, now we are registered with the server and can start looking for peers
    case d:Dev => {
      println("*** (httpActor) Got my device ID! ***")
      devID = d.devID
      context.parent ! getPeers()
    }
    
    //This means the remote actor indicated by devID is listening for connections on port port 
    case ACK(devID,port) => {
      println(s"Received (ACK) from remote device $devID and remote port $port")
      parent ! CON_OUT(devID,port.toInt) //Send device ID to main actor. This means that actor should try to connect to the remote host
    }
    
    //Sending an acknowledgement. This means the main actor is listening for connections from device d on port port
    case ACK_CON(d,port) => {
      val dID = d.devID
      println(s"Received (ACK_CON) from remote device $dID and remote port $port")
      sendAck(d,port)
    }
    
    /**
     * This message comes when a remote host is asking to connect
     * It is the first encounter with a remote host over the relay
     */
    case ASK_CON(devID,port) => {
      println(s"Received (ASK_CON) from remote device $devID and remote port $port")
      parent ! CON_IN(devID,port.toInt)
    }
    
    case s: String => {
      println("httpMsg: "+s)
    }
  }
}

/**
 * Registers this device on tracker, then listens for messages
 */
class httpListener(uid:String,trackerHost:String) extends Actor {
  import context._
  val config = Config(connectTimeout = 10000,
    readTimeout = 60000)
  
  var devID = ""
    
  def refresh() ={
    try{
	  val jsonDev = register()
	  println(jsonDev)
	  val thisDev = Dev.fromJSON(jsonDev)
	  Shared.setDev(thisDev)
	  devID = thisDev.devID
	  context.parent ! thisDev
    }
    catch{
      case e:ConnectException => throw new ConnectException(e.getMessage())
    }
  }
  
  refresh()
  self ! LISTEN(devID)
  
  def register():String = {
    def http = new HttpClient(config)
    val url = s"$trackerHost/u/$uid"
    val req = RequestBody(Map("IP"->getLocalIP()))
    val res = http.post(url, Some(req))
    return res.body.asString
  }
  def getLocalIP():String = {
    try{
    	val ipMatch = "/(.*):.*".r
    	val sock = new DatagramSocket()
    	sock.connect(InetAddress.getByName("gmail.com"),80) //Known always up host
    	val out = sock.getLocalSocketAddress().toString match {
    		case ipMatch(ipAddr) => ipAddr
    		case _ => "unknown"
    	}
    	return out
    }
    catch {
      case s:SocketException => {return "unknown"}
    }
  }

  def listen(devID:String):String = {
    def http = new HttpClient(config)
    val url = s"$trackerHost/p/$devID"
    val res = http.get(new URL(url))
    return res.body.asString
  }
  //val http200 = "OK"
  def receive = {
    case LISTEN(dev) => {
      println(s"listening as: $dev")
      listen(dev) match {
        case "OK" => {
        }
        case "Forbidden" => {
          println("***(HTTPListener) 403 Forbidden, someone else has this device ID? ***")
          throw new ForbiddenException("403: Forbidden")
        }
        case "Not Found" => {
           println("***(HTTPListener) 404 Not Found, refreshing device ***")
           throw new DevNotFoundException("Unexpectedly could not find device entry, restarting")
        }
        
        case s:String => {
          JSON.parseFull(s) match {
            case Some(json) => {
              json.asInstanceOf[Map[String,String]] get "type" match {
                case Some(thing) => {
                  if(thing.equals("ask")) 
                    context.parent ! ASK_CON.fromJSON(s)
                  else if(thing.equals("ack"))
                    context.parent ! ACK.fromJSON(s)
                }
                case None =>
                  println("***(HttpListener) No type variable found, unknown command type")
                  throw new JSONException("Expected type attribute, got none")
              }
            }
            case None => {
              context.parent ! s
            }
          }
        }
      }
      self ! LISTEN(dev)
    }
  }
}

object httpTest{ //extends App {
  //val system = ActorSystem("RUDPSystem")
  //val x = system.actorOf(Props(new httpListener("default","http://smaugshideout.com:3000")), name = "test")
  val system = ActorSystem("RUDPSystem")
  val main = system.actorOf(Props(new httpActor("default","http://smaugshideout.com:3000")), name = "test")
    
    
    
}


