package share.protocol
import net.rudp._
import java.io._
import java.net._
import java.security.MessageDigest
import Stream._
import akka.actor.ActorSystem
import scala.concurrent._
import scala.concurrent.duration._

object Chunks {
  val md = MessageDigest.getInstance("SHA")

  
  type Chunk = (Array[Byte],Long,String)//Data,offset,digest
  type ChunkInfo = (Long,Long,String)//nBytes,offset,digest
  val CHUNK_INF = ":ci(.*):(.*):(.*)".r//nBytes,offset,digest
  
  type FileInfo = (Long,Int,Long,String)//nBytes,ChunkSize,offset,signature
  val FILE_INF = ":fLi(.*):(.*):(.*):(.*)".r//nBytes,ChunkSize,offset,signature
  
  val ACC = ":AkC(.*)".r
  
  def getSignature(f:RandomAccessFile):String = {
    return f.length().toString()
  }
  
  def readChunk(f:RandomAccessFile,nBytes:Int, offset:Long = 0):Chunk = {
    f.seek(offset)
    val buf = new Array[Byte](nBytes)
    val len = f.read(buf)
    val outData = buf.slice(0,len)
    return (outData,offset,new String(md.digest(outData)))
  }
  
  def getCmd(cmdSig:Any,is:InputStream):Any = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case ACC(msg) => {
        return msg
      }
      case _ => {
        throw new IOException("Unknown acc format")
      }
    }
  }
  
  def sendAcc(msg:String="AOK",os:OutputStream) = {
    val out = new PrintWriter(os, true)
    out.println(":AkC"+msg)
  }
  def getAcc(is:InputStream):String = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case ACC(msg) => {
        return msg
      }
      case _ => {
        throw new IOException("Unknown acc format")
      }
    }
  }
  
  def sendFileInfo(f:RandomAccessFile,os:OutputStream,offset:Long = 0,chunkSize:Int = 1024000)={
    val out = new PrintWriter(os, true)
    val nBytes = f.length()
    val sig = getSignature(f)
    out.println(s":fLi$nBytes:$chunkSize:$offset:$sig")
  }
  
  def getFileInfo(is:InputStream):FileInfo = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case FILE_INF(nBytes,chunkSize,offset,signature) => {
        return (nBytes.toLong,chunkSize.toInt,offset.toLong,signature)
      }
      case _ =>{
        throw new IOException("Unrecognized File Info Format")
      }
    }
  }
  
  def sendFile(os:OutputStream,f:RandomAccessFile,offset:Long) = {
    f.seek(offset)
    val fin = new FileInputStream(f.getFD())
    wireCodes.copy(fin, os)
  }
  
  //GetFile
  
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

  def getChunkInfo(ch:Chunk):ChunkInfo = {
    val nBytes = ch._1.length
    val offset = ch._2
    val digest = ch._3
    return (nBytes,offset,digest)
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


class chunkFileGetter(s:Socket) {
  
}

/**
 * Sends chunks in files across RUDP (or any socket). Does SHA hash checking to ensure accurate transfer.
 * Methods:
 * 	send(RandomAccessFile) -> Sends a file in chunks, verifying recept
 *  						  Throws SocketException if connection is interrupted , gives offset so transfer can be renewed
 * 	
 */
class chunkFileSender(s:Socket,chunkSize:Int=1024000) {
  val in = s.getInputStream()
  val out = s.getOutputStream()
  def chunkinator(f:RandomAccessFile,offset:Long):Stream[Chunks.Chunk] = {
    //readChunk(f:RandomAccessFile,nBytes:Int, offset:Long = 0):Chunk
    Chunks.readChunk(f,chunkSize,offset) #:: chunkinator(f,offset+chunkSize)
  }
  def send(resource:String,offset:Long=0):Int = {
    val f = new RandomAccessFile(resource,"rw")
    f.seek(offset)
    return 1
    
    
    
    //0: Fix get/set cmd so errors can easily be built in later
    //1: sendFileInfo(file)
    //2: getAcc("GotFileInfo")
    //3: sendAcc("ReadyToSend")
    //4: getAcc("ReadyToGet")
    //5: chunkinator = new Chunkinator(myFile)
    //6: while chunkinator.hasNextChunk:
    //7: sendChunk(chunkinator.getNextChunk())
    //8: getAcc("chunkRecieved") | getAcc("chunkError") || SocketError | timeout <-this is why commnads need to be fixed
    //9: endwhile
    //10: getAcc("allFileRecieved")
    //11: return
  }
}


