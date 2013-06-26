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

abstract class msg
case class LISTEN(dev:String)

/**
 * Registers this device on tracker, then listens for messages
 */
class httpListener(uid:String,trackerHost:String) extends Actor {
  val config = Config(connectTimeout = 10000,
    readTimeout = 60000)
  def http = new HttpClient(config)
  val devID = register()
  self ! LISTEN(devID)
  def register():String = {
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
  def getPeers():String = {
    val url = s"$trackerHost/u/$uid"
    val res = http.get(new URL(url))
    return res.body.asString
  }
  def listen(devID:String):String = {
    val url = s"$trackerHost/p/$devID"
    val res = http.get(new URL(url))
    return res.body.asString
  }
  val http200 = "OK"
  def receive = {
    case LISTEN(dev) => {
      println(s"listening as: $dev")
      listen(dev) match {
        case http200 => {
          self ! LISTEN(dev)
        }
      }
    }
  }
}

object httpTest extends App {
  val system = ActorSystem("RUDPSystem")
  val x = system.actorOf(Props(new httpListener("default","http://smaugshideout.com:3000")), name = "test")
  
}