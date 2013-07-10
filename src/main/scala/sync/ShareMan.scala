package share.sync
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._



class ShareMan(protocol:ActorRef) extends Actor {
  import context._
  val myShares = new ShareContainer()
  val watcher = new LocalListener()
  def receive = {
    case _ =>
  }
}