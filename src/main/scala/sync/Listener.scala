package share.sync
import scala.collection.JavaConversions._
import name.pachler.nio.file._;
import java.io.IOException

case class fileEvent(kind:String,dir:String,file:String)

/**
 * Listens for local directory changes. Not recursive.
 */
class LocalListener() {
  val watchService = FileSystems.getDefault().newWatchService();
  var watched = Map[String,WatchKey]()
  var watchedInv = Map[WatchKey,String]()
  @throws(classOf[IOException])
  @throws(classOf[UnsupportedOperationException])
  def register(path:String)={
    val watchedPath = Paths.get(path)
    var key = null.asInstanceOf[WatchKey]
    key = watchedPath.register(watchService,StandardWatchEventKind.ENTRY_CREATE, StandardWatchEventKind.ENTRY_DELETE, StandardWatchEventKind.ENTRY_MODIFY)
    watched += (path->key)
    watchedInv += (key->path)
  }
  
  def rm(root:String) {
    watched get root match {
      case Some(key) => {
        key.cancel()
        watched -= root
        watchedInv -= key
        }
      case None => 
    }
  }
  
  def getNextChange():List[fileEvent] = {
    val signalledKey = watchService.take() // take event, blocks till event happens
    val events = signalledKey.pollEvents()
    signalledKey.reset()
    
    events.toList.map((x:WatchEvent[_])=>{
      val kind = x.kind().toString()
      val file = x.context().toString()
      val dir = watchedInv.getOrElse(signalledKey,"")
      fileEvent(kind,file,dir)
    })
  }
}

object fileChanger{// extends App {
  val ll = new LocalListener()
  ll.register("/home/josh")
  while(true){
    val events = ll.getNextChange()
    println(events(0).toString())
    
    Thread.sleep(1000)
  }
}
