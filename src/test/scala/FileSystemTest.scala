import org.scalatest.FunSuite
import share.sync._
import java.io.File
class FileTreeTest extends FunSuite {
  test("Test Make FileTree") {
    expect(3) { 
      val x = new File("src/test/scala/dirs/dir1")
      val y = new FileTree(x)
      y.getChildren().length
      //new java.io.File(".").getCanonicalPath()
    }
  }
  test("Test Get All Files") {
    expect(8) { 
      val x = new File("src/test/scala/dirs/dir1_del")
      val y = new FileTree(x)
      y.getAll().length
      //new java.io.File(".").getCanonicalPath()
    }
  }
}

class DiffTest extends FunSuite {
  test("Test RMLOCAL"){
    expect(true){
    val a = new FileTree(new File("src/test/scala/dirs/dir1"))
    val r = a
    val b = new FileTree(new File("src/test/scala/dirs/dir1_del/dir1"))
    val f = funcs.diff(Option(a),Option(b),Option(a))
      var ret = false
      if(f.length == 1){
        println(f(0))
        ret = f(0) match {
          case funcs.RMLOCAL(something) => true
          case _ =>false
        }
        ret
      }
    }
  }
  def getDiff(x:String,y:String,z:String):List[funcs.Action] = {
    val a = new FileTree(new File(x))
    val b = new FileTree(new File(y))
    val r = new FileTree(new File(z))
    val f = funcs.diff(Option(a),Option(b),Option(r))
    return f
  }
  test("Test RMREMOTE"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1")
      var ret = false
      if(f.length == 1){
        println(f(0))
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
      val f = getDiff("src/test/scala/dirs/dir1_del/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1/dir1_del/dir1")
      var ret = false
      if(f.length == 1){
        println(f(0))
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
        println(f(0))
        ret = f(0) match {
          case funcs.PUT(something) => true
          case _ =>false
        }
        println(f)
        ret
      }
    }
  }
  test("Test CONFLICT"){
    expect(true){
      val f = getDiff("src/test/scala/dirs/dir1_changed/dir1","src/test/scala/dirs/dir1","src/test/scala/dirs/dir1_del/dir1")
      var ret = false
      if(f.length == 1){
        println(f(0))
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
        println(f(0))
        ret = f(0) match {
          case funcs.MISSING(something) => true
          case _ =>false
        }
        ret
      }
    }
  }  
}
