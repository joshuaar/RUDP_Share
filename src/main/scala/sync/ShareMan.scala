package share.sync
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import name.pachler.nio.file._

import scala.concurrent.stm._

object ShareMan {
  val myShares:Ref[Option[ShareContainer]] = Ref(None)
  def getShares():ShareContainer = {
    atomic { implicit txn =>
      myShares() match {
        case Some(d) => return d
        case None => return new ShareContainer()
      }
    }
  }
  
  def addShare(s:Share) = {
    atomic { implicit txn =>
      val oldShare = getShares()
      val newShare = oldShare.add(s)
      myShares() = Option(newShare)
    }
  }
  def rmShare(s:Share) ={
    atomic {implicit txn =>
      val oldShare = getShares()
      val newShare=oldShare.remove(s)
      
      myShares() = Option(newShare)
    }
  }
}

abstract class ShareManMsg
case class mkShare(root:String,name:String) extends ShareManMsg {
  def mk():Share = {
    val ft = FileTree.fromString(root)
    return new Share(ft,name,root)
  }
}

case class mkSync(root:String,name:String) extends ShareManMsg {
  def mk():Share = {
    val ft = FileTree.fromString(root)
    return new Share(ft,name,root,"Sync")
  }
}

case class rmShare(s:Share) extends ShareManMsg
case class getNext extends ShareManMsg
case class DirListenerException(msg:String) extends Exception
class ShareMan(protocol:String) extends Actor {
  import context._
  //val myShares = new ShareContainer()
  val watcher = new LocalListener()
  self ! new getNext()
  def handleEvent(f:fileEvent){
    println(f)
    
//    val x = Shared.getShares()
//    println(x.getAll.length)
//    Shared.addShare(new Share(FileTree.fromString("/home/josh/Documents"),"","",""))
//    val y = Shared.getShares()
//    val z =y.getAll()
//    println(z.length)
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
      //Register all directories with watch service
      share.getFileTree.ApplyToAllDirs(m.root,(x:String)=>watcher.register(x))
      println("adding share to collection")
      ShareMan.addShare(share)
      //watcher.register(m.root)
      println("finished adding share to collection")
    }
    
    case g:getNext => {
      getNext()
    }
    
    case rmShare(s:Share) => {
      s.getFileTree.ApplyToAllDirs(s.getRoot, (x:String)=>watcher.rm(x))
      ShareMan.rmShare(s)
      //watcher.rm(s.getRootDir())
      
    }
  }
}

object shareManTest extends App {
  println("Creating test share")
  FileTree.fromString("/home/josh/Downloads")
  val system = ActorSystem("ShareMan")
  val shares = system.actorOf(Props(new ShareMan("myProtocol")), name = "shareManTest")
  shares ! mkShare("/home/josh/Downloads","homeShare")
  Thread.sleep(5000)
}