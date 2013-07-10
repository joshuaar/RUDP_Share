package share.sync
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import name.pachler.nio.file._


//class LocalListener() {
//  
//}
//  @throws(classOf[IOException])
//  @throws(classOf[UnsupportedOperationException])
//  def registerWatchedPath(path:String)={
//
//  def getNextChange():List[WatchEvent[_]]={
//

abstract class ShareManMsg
case class mkShare(root:String,name:String) extends ShareManMsg {
  def mk():Share = {
    val ft = new FileTree(new File(root))
    return new Share(ft,name,root)
  }
}

case class mkSync(root:String,name:String) extends ShareManMsg {
  def mk():Sync = {
    val ft = new FileTree(new File(root))
    return new Sync(ft,name,root)
  }
}

case class rmShare(s:Share) extends ShareManMsg
case class getNext extends ShareManMsg
case class lsShare extends ShareManMsg
case class lsSync extends ShareManMsg
case class DirListenerException(msg:String) extends Exception
class ShareMan(protocol:String) extends Actor {
  import context._
  val myShares = new ShareContainer()
  val watcher = new LocalListener()
  self ! new getNext()
  def handleEvent(f:fileEvent){
    println(f)
  }
  
  def getNext()={
    println("Getting next folder event")
    val future = Future{watcher.getNextChange()}
    future onSuccess {
      case x:List[fileEvent]=>{
        for(i <- x) 
          handleEvent(i)
        self ! new getNext
      }
      case _=>{
        throw new DirListenerException("Unknown Error with share manager")
      }
    }
    
    future onFailure {
      case e => println("An error has occured with the watch service: "+e.getStackTraceString+e.getMessage())
    }
  }
  
  def receive = {
    case m:mkShare => { 
      println("creating share")
      val share = m.mk()
      println("adding share to collection")
      myShares.add(share)
      //watcher.register(m.root)
      share.files.ApplyToAllDirs(m.root,(x:String)=>watcher.register(x))
      println("finished adding share to collection")
    }
    
    case g:getNext => {
      getNext()
    }
    
    case rmShare(s:Share) => {
      myShares.remove(s)
      //watcher.rm(s.getRootDir())
      s.files.ApplyToAllDirs(s.getRootDir, (x:String)=>watcher.rm(x))
    }
  }
}

object shareManTest extends App {
  println("Creating test share")
  new FileTree(new File("/home/josh/Downloads"))
  val system = ActorSystem("ShareMan")
  val shares = system.actorOf(Props(new ShareMan("myProtocol")), name = "shareManTest")
  shares ! mkShare("/home/josh/Downloads","homeShare")
  Thread.sleep(5000)
}