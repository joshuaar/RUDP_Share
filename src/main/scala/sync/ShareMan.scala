package share.sync
import java.net._
import share.protocol.rudp.api
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
//import name.pachler.nio.file._

import scala.concurrent.stm._

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
case class getShareJSON() extends ShareManMsg//gets

case class rmShare(s:Share) extends ShareManMsg//gets

case class getNext extends ShareManMsg//internal use

case class shareMod(shareName:String,action:String,path:List[String])//emits

case class subscribe(actor:ActorRef)//gets

case class DirListenerException(msg:String) extends Exception
class ShareMan extends Actor {
  import context._
  var subscribers = Set[ActorRef]()
  val myShares = new ShareContainer()
  val watcher = new LocalListener()
  self ! new getNext()
  
  def handleEvent(f:fileEvent){
    implicit def str2list(s:String):List[String] = {
      val out = s.split(File.separator).toList
      if(out.head.equals("")){//For instances where the input has a leading / (/home/josh/stuff/whatever)
        return out.tail
      }
      return out
    }

    val share = myShares.matchShare(f.dir)
    println(myShares.getAll().map(_.getRoot))
    
    println("fileEvent Directory: "+f.dir)
    
    share match {
      
      case Some(ftshare) => {
        
        //ftshare.getRoot //the root of the share(should be a superdirectory f.dir)
        println("matcher: "+ftshare.getRoot+"(.*)")
        val matcher = (ftshare.getRoot+"(.*)").r
        
        f.dir match {
          case matcher(suffix) =>{
            val subTreeAddr = str2list(ftshare.getRoot).last + File.separator + str2list(suffix).mkString(File.separator)
            
            println(ftshare.getFileTree().getNodeName)
            println(str2list(subTreeAddr))
            if(f.kind.equals("ENTRY_CREATE")){
              val ft = try{
            	  FileTree.fromFileEvent(f)
              }
              catch {
                case e:SubtreeRootException => {
                  println("subtree creation failed, ignoring this event")
                  return 
                }
              }
            	ftshare.getFileTree().putSubtree(subTreeAddr, ft)
            	println(str2list(subTreeAddr))
            	println(ftshare.getFileTree().getSubtree(str2list(subTreeAddr)++List(f.fileName)))
            	if(ftshare.getFileTree.getNode.isDirectory)//Register directory change
            	  
            		ft.ApplyToAllDirs(f.dir+File.separator+ft.getNodeName, (x:String)=>watcher.register(x))//Register change
            }
            
            else if(f.kind.equals("ENTRY_DELETE")){
              val rmAddr = str2list(subTreeAddr)++List(f.fileName)
              println(rmAddr)
              val ft = ftshare.getFileTree().getSubtree(rmAddr)
              try{
              ft.get.ApplyToAllDirs(f.dir+File.separator+ft.get.getNodeName, (x:String)=>watcher.rm(x))//remove from watch service
              println("Entry deleted")
              }
              catch {
                case e:java.util.NoSuchElementException => {
                  println("watch deletion failed, ignoring this event")
                  return
                }
              }
              ftshare.getFileTree().removeSubtree(rmAddr)
            }
            
            else if(f.kind.equals("ENTRY_MODIFY")){
              val modAddr = str2list(subTreeAddr)++List(f.fileName)
              val st = ftshare.getFileTree().getSubtree(modAddr)
              st match {
                case Some(thing) => {
                  println("Modifying Entry")
                  try{
                    
                	  thing.setRoot(fileObj.fromPathString(f.dir+File.separator+f.fileName))
                	  println("Entry has been modified")
                  }
                  catch {
                    case e:FileObjException => {
                      println("File does not exist, ignoring event. Entry should be deleted here?")
                      return
                    }
                  }
                  }
                case None => {
                  return
                }
              }
            }
            
            for(i<-subscribers)//Send the change to all subscribers
              i ! shareMod(ftshare.getName,f.kind,f.dir)
              }
          }
        }
      
      case None => throw new Exception("Share not found, cannot insert tree")
      
    	}
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
        println(x)
        for(i <- x) 
          handleEvent(i)
        self ! new getNext
      }
      case _=>{
        throw new DirListenerException("Unknown Error with share manager")
      }
    }
    
    future onFailure {
      case e => {
        println("An error has occured with the watch service: ")
        throw e
        }
    }
  }
  
  def receive = {
    case m:mkShare => { 
      println("creating share")
      val share = m.mk()
      //Register all directories with watch service
      share.getFileTree.ApplyToAllDirs(m.root,(x:String)=>watcher.register(x))
      println("adding share to collection")
      myShares.add(share)
      //watcher.register(m.root)
      println("finished adding share to collection")
    }
    
    case g:getShareJSON => {
      sender ! myShares.toString()
    }
    
    
    case subscribe(actor) => {
      subscribers += actor
      //sender ! myShares.toString()
    }
    
    case g:getNext => {
      getNext()
    }
    
    case rmShare(s:Share) => {
      s.getFileTree.ApplyToAllDirs(s.getRoot, (x:String)=>watcher.rm(x))
      myShares.remove(s)
      //watcher.rm(s.getRootDir())
      
    }
  }
}

object shareManTest extends App {
  println("Creating test share")
  FileTree.fromString("/home/josh/Downloads")
  val system = ActorSystem("ShareMan")
  val shares = system.actorOf(Props(new ShareMan()), name = "shareManTest")
  shares ! mkShare("/home/josh/Downloads","homeShare")
  Thread.sleep(5000)
}