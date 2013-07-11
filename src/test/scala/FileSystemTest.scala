import org.scalatest.FunSuite
import share.sync._
import java.io.File
class FileTreeCreate extends FunSuite {
  test("Test Make FileTree") {
    expect(3) { 
      val x = new File("src/test/scala/dirs/dir1")
      val y = FileTree.fromFile(x)
      y.getChildren().values.toList.length
      //new java.io.File(".").getCanonicalPath()
    }
  }
  test("Test Get All Files") {
    expect(8) { 
      val x = new File("src/test/scala/dirs/dir1_del")
      val y = FileTree.fromFile(x)
      y.getAll()
      //new java.io.File(".").getCanonicalPath()
    }
  }
}

class FileTreeSerialize extends FunSuite {
  test("Test JSON Serialization") {
    expect(true) {
       val a = FileTree.fromFile(new File("src/test/scala/dirs/dir1"))
       val b = a.toJSON
       val c = FileTree.fromJSON(b)
       a.toJSON().equals(c.toJSON())
    }
  }
}

class DiffTest extends FunSuite {
  val ref = FileTree.fromFile(new File("src/test/scala/dirs"))
  test("Test Get Subtree"){
    expect(Set("dir1","file1","file2","dir2")){
      ref.getSubtree(List("dir1_del","dir1")) match {
        case Some(tree) => tree.listChildNames().toSet+tree.getNode().getName
        case None => false
      }
    }
  }
  test("Test RMLOCAL"){
    expect(true){
    val a = FileTree.fromFile(new File("src/test/scala/dirs/dir1"))
    val r = a
    val b = FileTree.fromFile(new File("src/test/scala/dirs/dir1_del/dir1"))
    val f = funcs.diff(Option(a),Option(b),Option(a))
      var ret = false
      if(f.length == 1){
        //println(f(0))
        ret = f(0) match {
          case funcs.RMLOCAL(something) => true
          case _ =>false
        }
        ret
      }
    }
  }
  def getDiff(x:String,y:String,z:String):List[funcs.Action] = {
    val a = FileTree.fromFile(new File(x))
    val b = FileTree.fromFile(new File(y))
    val r = FileTree.fromFile(new File(z))
    val f = funcs.diff(Option(a),Option(b),Option(r))
    return f
  }
  test("Test RMREMOTE"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1")
      var ret = false
      if(f.length == 1){
        //println(f(0))
        ret = f(0) match {
          case funcs.RMREMOTE(something) => true
          case _ =>false
        }
        ret
      }
    }
  }
  test("Test GET"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1_del/dir1")
      var ret = false
      if(f.length == 1){
       // println(f(0))
        ret = f(0) match {
          case funcs.GET(something) => true
          case _ =>false
        }
        ret
      }
    }
  }
  
  test("Test PUT"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1","src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1_del/dir1")
      var ret = false
      if(f.length == 1){
        //println(f(0))
        ret = f(0) match {
          case funcs.PUT(something) => true
          case _ =>false
        }
        //println(f)
        ret
      }
    }
  }
  test("Test CONFLICT"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_changed/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1_del/dir1")
      var ret = false
      if(f.length == 1){
       // println(f(0))
        ret = f(0) match {
          case funcs.CONFLICT(something,elses) => true
          case _ =>false
        }
        ret
      }
    }
  }
  test("Test MISSING"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1")
      var ret = false
      if(f.length == 1){
       // println(f(0))
        ret = f(0) match {
          case funcs.MISSING(something) => true
          case _ =>false
        }
        ret
      }
    }
  }  
}
