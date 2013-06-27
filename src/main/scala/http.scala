package share.protocol.http
import akka.actor._
import java.net._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import uk.co.bigbeeconsultants.http._
import uk.co.bigbeeconsultants.http.response.Response
import uk.co.bigbeeconsultants.http.request.RequestBody
import java.net.URL
import scala.util.parsing.json._

case class Dev(ipLocal:String,ipExternal:String,devID:String)
case class JSONException(msg:String) extends Exception
case class ConnException(msg:String) extends Exception
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
  var devID = ""
    
  val config = Config(connectTimeout = 10000, readTimeout = 60000)
  def http = new HttpClient(config)
  val listener = context.actorOf(Props(new httpListener(uid,trackerHost)))
  
  //context.parent ! getPeers // send initial peers list to main actor
  
  //wire codes
  val ASK_CON = ":AsK:(.*):(.*)".r//devID:port
  val ACK = ":AcK:(.*):(.*)".r//acknowledgement with message
  
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
          i => Dev(i getOrElse ("ipLocal","nil"), 
          i getOrElse("ipExternal","nil"), 
          i getOrElse("devID","nil")))
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
    case DEVID(id) => {
      println("*** (httpActor) Got my device ID! ***")
      devID = id
      context.parent ! getPeers()
    }
    
    case ACK(devID,port) => {
      parent ! CON_OUT(devID,port.toInt) //Send device ID to main actor. This means that actor should try to connect to the remote host
    }
    
    case ACK_CON(d,port) => {
      sendAck(d,port)
    }
    
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
	  devID = register()
	  context.parent ! DEVID(devID)
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


