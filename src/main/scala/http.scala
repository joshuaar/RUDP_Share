package share.protocol.http
import akka.actor._
import java.net._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.AllForOneStrategy
import akka.actor.SupervisorStrategy._

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
abstract class msg
case class LISTEN(dev:String) extends msg
case class SEND(s:String,dev:String) extends msg
case class DEVID(id:String) extends msg
case class GETPEERS extends msg
case class REQ_CON(d:Dev,port:Int)
case class CON_OUT(devID:String,port:Int)
case class CON_IN(devID:String,port:Int)
case class ACK_CON(devID:Dev,port:Int)

class httpActor(uid:String, trackerHost:String) extends Actor {
  import context._
  //Supervisor Strategy
  override val supervisorStrategy = 
     AllForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 2 minute) {
    case _:ForbiddenException => {
      Thread.sleep(10000)
      Restart
      }
    case _:Exception => Escalate
}
  
  
  var devID = ""
    
  val config = Config(connectTimeout = 10000, readTimeout = 60000)
  def http = new HttpClient(config)
  val listener = context.actorOf(Props(new httpListener(uid,trackerHost)))
  
  //context.parent ! getPeers // send initial peers list to main actor
  
  //wire codes
  val ASK_CON = ":AsK:(.*):(.*)".r//devID:port
  val ACK = ":AcK:(.*):(.*)".r//acknowledgement with message
  
  /**
   * This gets picked up by the ASK_CON handler
   */
  def reqConnect(d:Dev,port:Int) = {
    val res = sendMsg(d.devID,s":AsK:$devID:$port")
  }
  
  def sendAck(d:Dev,port:Int) = {
    if(devID==""){
      throw new Exception("We don't have a local device id!")
    }
    val res = sendMsg(d.devID,s":AcK:$devID:$port")
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
    val url = s"$trackerHost/p/$dev"
    val req = RequestBody(Map("msg"->msg))
    val res = http.post(url,Some(req))
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
      parent ! CON_OUT(devID,port.toInt) //Send device ID to main actor. This means that actor should try to connect to the remote host
    }
    
    //Sending an acknowledgement. This means the main actor is listening for connections from device d on port port
    case ACK_CON(d,port) => {
      sendAck(d,port)
    }
    
    /**
     * This message comes when a remote host is asking to connect
     * It is the first encounter with a remote host over the relay
     */
    case ASK_CON(devID,port) => {
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
	  val jsonDev = register()
	  println(jsonDev)
	  val thisDev = Dev.fromJSON(jsonDev)
	  Shared.setDev(thisDev)
	  devID = thisDev.devID
	  context.parent ! thisDev
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
        case s:String => {
          context.parent ! s
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


