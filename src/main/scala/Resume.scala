package share.protocol
import net.rudp._
import java.io._
import java.net._
import java.security.MessageDigest

object Chunks {
  val md = MessageDigest.getInstance("SHA")
  val CHUNK_INF = ":ci(.*):(.*):(.*)".r//nBytes,offset,digest
  type Chunk = (Array[Byte],Long,String)//Data,offset,digest
  type ChunkInfo = (Long,Long,String)//nBytes,offset,digest
  
  def readChunk(f:RandomAccessFile,nBytes:Int, offset:Long = 0):Chunk = {
    f.seek(offset)
    val buf = new Array[Byte](nBytes)
    val len = f.read(buf)
    val outData = buf.slice(0,len)
    return (outData,offset,new String(md.digest(outData)))
  }
  
  def getChunkInfo(ch:Chunk):ChunkInfo = {
    val nBytes = ch._1.length
    val offset = ch._2
    val digest = ch._3
    return (nBytes,offset,digest)
  }
  
  def sendChunkInfo(os:OutputStream,ch:Chunk) = {
    val out = new PrintWriter(os, true)
    val nBytes = ch._1.length
    val offset = ch._2
    val digest = ch._3
    out.println(s":ci$nBytes:$offset:$digest")
  }
  
  def getChunkInfo(is:InputStream):ChunkInfo = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case CHUNK_INF(nBytes,offset,digest) => {
        return (nBytes.toLong,offset.toLong,digest)
      }
      case _ =>{
        throw new IOException("Unrecognized Chunk Info Format")
      }
    }
  }
  
  def sendChunk(out:OutputStream,ch:Chunk,bufSize:Int=2048):Long = {
    val nBytes = ch._1.length
    if(nBytes <= bufSize){
    	out.write(ch._1)
    	return nBytes
    }
    val in = new ByteArrayInputStream(ch._1)
    wireCodes.copy(in,out)
  }
  
  def getChunk(in:InputStream,chInf:ChunkInfo,bufSize:Int=1024):Chunk = {
    val nBytes = chInf._1
    val digest = chInf._3
    val offset = chInf._2
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](bufSize)
    var gotten:Long = 0
    var len:Int = 0
    len = in.read(buf)
    while(len != -1 && gotten < nBytes) {
      out.write(buf,0,len)
      gotten+=len
      len = in.read(buf)
    }
    var result = out.toByteArray()
    if(nBytes != -1)
      result = result.slice(0,nBytes.toInt)
    val resultDigest=new String(md.digest(result))
    val refDigest = new String(digest)
    
    if(refDigest != resultDigest){
      val gotten = new String(result)
      throw new IOException(s"Hashes do not match, chunk transfer failed. got: $gotten ")
    }
    return (result,offset,digest)
  }
  
  def saveChunk(f:RandomAccessFile,ch:Chunk) = {
    f.seek(ch._2) //goto offset
    f.write(ch._1) //write the chunk
  }
}


