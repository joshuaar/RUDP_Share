package share.sync
//import java.io.File
import scala.util.parsing.json._
import java.io._

trait Types {
  type Sort = (String, Long)
  type Children = Array[FileTree]
  type JSONNode = Map[String,List[Any]]
  implicit def arrayToList[A](a: Array[A]) = a.toList
}

case class fileObj(isDirectory:Boolean,getName:String,length:Long) {
  def toJSON():String = {
    s""""m":[$isDirectory,"$getName",$length]"""
  }
}

object FileTree {
  def fromByteArray(a:Array[Byte]):FileTree = {
    val bis = new ByteArrayInputStream(a)
    val in = new ObjectInputStream(bis)
    val output = in.readObject()
    return output.asInstanceOf[FileTree]
  }
}

/**
 * Builds an abstract file tree from a root directory
 */
@serializable
class FileTree(f: File) extends Types{
  val test = "hello"
  private var root = makeRoot(f)
  private var children = Array[FileTree]()
  if(f.isDirectory())
    children = buildTree()
  def buildTree(): Children = {
    val children = f.listFiles().map((x: File) => new FileTree(x))
    return children
  }
  
  def toJSON():String = {
    val rootJSON = root.toJSON()
    val childrenJSONChunks = children.map((x:FileTree) => x.toJSON())
    var childrenJSON = ""
    if(childrenJSONChunks.length == 0){}
    else if(childrenJSONChunks.length == 1){
      childrenJSON = childrenJSONChunks(0)
    }
    else {
      childrenJSON = childrenJSONChunks.reduce((x:String,y:String) => s"$x,$y")
    }
    s"""{$rootJSON,"c":[$childrenJSON]}"""
  }
  
  def toByteArray():Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(this)
    out.close()
    val output = bos.toByteArray()
    bos.close()
    output
  }
  
  def makeRoot(f:File):fileObj = {
    return fileObj(f.isDirectory(),f.getName(),f.length())
  }
  
  def listChildNames(): Array[String] = {
    return children.map(_.root.getName)
  }
  def getChildren(): Children = {
    return children
  }
  def getChildDirs(): Children = {
    return children.filter(_.root.isDirectory)
  }
  def getChildDirNames(): Array[String] = {
    return getChildren.map(_.root.getName)
  }
  def getSort():Sort = {
    return (root.getName, root.length)
  }
  def getNode():fileObj = {
    return root
  }
  def getAll():List[fileObj] = {
    var x = root
    val ch = children
    root :: ch.flatMap(_.getAll())
  }
}

object funcs extends Types {
  
  /**
   * Actions generated to fix out of sync files
   */
  abstract class Action
  case class PUT(what: fileObj) extends Action
  case class GET(what: fileObj) extends Action
  case class RMREMOTE(what: fileObj) extends Action
  case class RMLOCAL(what: fileObj) extends Action
  case class CONFLICT(local: fileObj, remote: fileObj) extends Action
  case class MISSING(what: fileObj) extends Action
  
  /**
   * These functions check if resources are synced or not by testing equality
   */
  def isSynced(a:Option[Sort],b:Option[Sort]):Boolean = return a==b
  def isSynced(a:Option[Sort],b:Sort):Boolean = a==b
  
   /**
   * Detects differences between this file tree and another.
   * Uses a reference tree to find out which tree updated the file:
   * Here are the possibilities: ( a ~ b -> file/dir a is in sync with file/dir b according to some function that determines this)
   * 	
   * 	a~b = this ~ other	a~o = this ~ reference	o~r = other ~ reference
   * 
   * 	a~b ^ a~r ^ b~r -> no changes anywhere since last sync
   * 	!a~b ^ a~r ^ !b~r -> remote was changed, get it from the remote
   * 	!a~b ^ !a~r ^ b~r -> local was changed, put it to the remote
   * 	!a~b ^ !a~r ^ !b~r -> Conflict! both files changed! Put the older one in a sep. folder
   * 
   * 	... else ... Errors:
   *	a~b ^ a~o ^!o~r -> Problem with reference. Syncs by external programs: Do sync as if first time..
   *	a~b ^!a~o ^ o~r -> Problem with reference. Syncs by external programs?
   *	...
   *	
   */
  def diff(thisTree: Option[FileTree], otherTree: Option[FileTree], refTree: Option[FileTree]):List[Action] = {

    if(thisTree.isEmpty && otherTree.isEmpty && refTree.isEmpty){ // if all trees are empty, return empty
      return List.empty[Action]
    }
    
    var a = thisTree map (_.getSort())
    var b = otherTree map (_.getSort())
    val r = refTree map (_.getSort())
    
    if(a.isEmpty){ // a is empty
      if(b.isEmpty)
        //throw new Exception("Fuq Scala")
        return MISSING(refTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
        
      else if(isSynced(b,r)) // a was deleted locally
        return RMREMOTE(otherTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
        
      else if(isSynced(a,r)) // b was created remotely
        return GET(otherTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    }
    
    else if(b.isEmpty){ // b is empty, a is not
      if(isSynced(a,r)){ // b was deleted remotely
        return RMLOCAL(thisTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
      }
      else //if(isSynced(b,r)) // a was created locally
        return PUT(thisTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    
    }    
    else if(isSynced(a,b)){ // a and b nonempty from now on
      return diffChild(thisTree, otherTree, refTree)
    }
    else if(r.isEmpty){ // a and b have contents, but r is empty
    	return CONFLICT(thisTree.get.getNode(),otherTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    }
    else if(isSynced(a,r) && !isSynced(b,r)){ // b has changed or absent
      return GET(otherTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    }
    else if(!isSynced(a,r) && isSynced(b,r)){ // a has changed
      return PUT(thisTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    }
    else if(!isSynced(a,r) && !isSynced(b,r)){ // both have changed
      return CONFLICT(thisTree.get.getNode(),otherTree.get.getNode()) :: diffChild(thisTree,otherTree,refTree)
    }
    
    else { // error, possibly corrupted reference
      throw new Exception("Fatal, Possibly corrupted reference")
    }
    return List.empty[Action]
    
  }
  
  def diffChild(thisTree: Option[FileTree], otherTree: Option[FileTree], refTree: Option[FileTree]):List[Action] = {
    val thisChildren = thisTree map (_.getChildren())
    val otherChildren = otherTree map (_.getChildren())
    val refChildren = refTree map (_.getChildren())
    
    val a = thisChildren match{
      case Some(trees) => trees.map(_.getSort()._1)
      case None => Array[String]()
    }
    val b = otherChildren match{
      case Some(tree) => tree.map(_.getSort()._1)
      case None => Array[String]()
    }
    val r = refChildren match{
      case Some(tree) => tree.map(_.getSort()._1)
      case None => Array[String]()
    }
    // a,b, and r are the sorts for this, other and ref respectively
    //get children and descendents diff'd
    val all = (b ++ a ++ r).toSet
    
    //Gets child matching a particular sort's !name!
    def getChild(s:String, lib:Option[Children]):Option[FileTree] = {
      if(lib.isEmpty)
        return Option.empty[FileTree]
      val out = lib.get.filter(_.getSort()._1 == s) // Get the fileTrees that have the same sort as the query 
      
      if(out.length == 0)
        return Option.empty[FileTree]
      
      if(out.length == 1)
        return Option(out(0))
        
      throw new Exception("Fatal, Multiple files with same name in "+out.toString())
    }
    
    val childPairs = all.map(x => (getChild(x,thisChildren).map(_.getSort()),getChild(x,otherChildren).map(_.getSort()),getChild(x,refChildren).map(_.getSort())))

    
    //Get the files in the corresponding filesystems, or get none if missing
    val preout = all.map( x => diff(getChild(x,thisChildren),getChild(x,otherChildren),getChild(x,refChildren)) )
    
    
    return preout.foldLeft(List.empty[Action])((x:List[Action],y:List[Action]) => x ++ y)
  }
}

object run extends App{
  def getDiff(x:String,y:String,z:String):List[funcs.Action] = {
    val a = new FileTree(new File(x))
    val b = new FileTree(new File(y))
    val r = new FileTree(new File(z))
    val f = funcs.diff(Option(a),Option(b),Option(r))
    return f
  }
  
  val a = new FileTree(new File("src/test/scala/dirs/dir1"))
  val b = a.toJSON()
  println(b)
  val c = a.toByteArray
  val d = FileTree.fromByteArray(c)
  println(d.toJSON)
   // val f = getDiff("/home/josh/Downloads","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1/dir1_del/dir1")
   // println("Printing Missing")
   // println(f.mkString("\n"))
//    val a = new FileTree(new File("src/test/scala/dirs/dir1"))
//    val r = a
//    val b = new FileTree(new File("src/test/scala/dirs/dir1_del/dir1"))
//    val z = funcs.diff(Option(a),Option(b),Option(a))
//    println(z.mkString("\n"))
//    var x = new FileTree(new File("/home/josh/Documents/"))
//    println(x.listChildNames() mkString "\n")
//    println(x.getChildDirNames().mkString("\n"))
//    val res = funcs.diff(Option.empty[FileTree],Option(x),Option(x))
//    println(res.mkString("\n"))
    //println(x.tree.root.list() mkString "\n")
    //val stuff = new File("/home/josh/Documents/").listFiles().map(_.listFiles()) 
    //println(stuff mkString "\n")
  }