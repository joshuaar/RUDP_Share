import org.scalatest.FunSuite
import share.protocol._
import java.io._
class ResumeSpec extends FunSuite {
  test("Test read chunk") {
    expect(true) {
      val f = new RandomAccessFile("src/test/scala/testFile","rw")//Make a file
      val ch = Chunks.readChunk(f, 10, 0)//Read chunk from file (file,nBytes,offset)
      val refFst = new String(Array[Byte](116, 101, 115, 116, 116, 101, 115, 116, 116, 101))
      val dataWorked = new String(ch._1) == refFst
      val offWorked = ch._2 == 0
      dataWorked && offWorked
    }
  }
  test("test send chunk info"){
    expect(true) {
      val f = new RandomAccessFile("src/test/scala/testFile","rw")//Make a file
      val ch = Chunks.readChunk(f, 10, 0)//Read chunk from file (file,nBytes,offset)
      var out = new ByteArrayOutputStream()//Make a place to send chunk info
      Chunks.sendChunkInfo(out,ch)//Send chunk info to output channel
      var input = out.toByteArray
      var in = new ByteArrayInputStream(input)//Turn chunk info into an input stream so it can be read
      val chInf = Chunks.getChunkInfo(in)//Read the chunk info
      val lenWorked = chInf._1 == ch._1.length
      val offWorked = chInf._2 == ch._2
      val hashWorked = chInf._3 == ch._3
      lenWorked && offWorked && hashWorked
    }
  }
  
  test("test send chunk"){
    expect(new String(Array[Byte](116, 101, 115, 116, 116, 101, 115, 116, 116, 101))){
      val f = new RandomAccessFile("src/test/scala/testFile","rw")//Make a file
      val ch = Chunks.readChunk(f, 10, 0)//Read chunk from file (file,nBytes,offset)
      val chInf = Chunks.getChunkInfo(ch)
      val out = new ByteArrayOutputStream()
      val nSent = Chunks.sendChunk(out,ch)
      new String(out.toByteArray())
    }
  }
  
  test("Test send/receive chunk over local streams") {
    expect(new String(Array[Byte](116, 101, 115, 116, 116, 101, 115, 116, 116, 101))) {
      val f = new RandomAccessFile("src/test/scala/testFile","rw")//Make a file
      val ch = Chunks.readChunk(f, 10, 0)//Read chunk from file (file,nBytes,offset)
      
      var out = new ByteArrayOutputStream()//Make a place to send chunk info
      Chunks.sendChunkInfo(out,ch)//Send chunk info to output channel
      
      var input = out.toByteArray()
      var in = new ByteArrayInputStream(input)//Turn chunk info into an input stream so it can be read
      val chInf = Chunks.getChunkInfo(in)//Read the chunk info
      
      out = new ByteArrayOutputStream()//A place to send the actual chunk
      Chunks.sendChunk(out, ch)//Send the actual chunk over the output stream
      input = out.toByteArray()
      in = new ByteArrayInputStream(input)//Place to read the chunk
      val chRead = Chunks.getChunk(in, chInf)
      new String(chRead._1)
    }
  }
}