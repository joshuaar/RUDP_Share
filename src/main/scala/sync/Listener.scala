package share.sync
import scala.collection.JavaConversions._
import name.pachler.nio.file._;
import java.io.IOException

/**
 * Listens for local directory changes. Not recursive.
 */
class LocalListener(path:String) {
  val watchService = FileSystems.getDefault().newWatchService();
  var watched = null.asInstanceOf[WatchKey]
  registerWatchedPath(path)
  
  
  @throws(classOf[IOException])
  @throws(classOf[UnsupportedOperationException])
  def registerWatchedPath(path:String)={
    val watchedPath = Paths.get(path)
    var key = null.asInstanceOf[WatchKey]
    key = watchedPath.register(watchService,StandardWatchEventKind.ENTRY_CREATE, StandardWatchEventKind.ENTRY_DELETE, StandardWatchEventKind.ENTRY_MODIFY)
  }
  
  def getNextChange():List[WatchEvent[_]]={
    val signalledKey = watchService.take() // take event, blocks till event happens
    val events = signalledKey.pollEvents()
    signalledKey.reset()
    return events.toList
  }
}
