package share.protocol
import net.rudp._
import java.io._
import java.net._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.security.MessageDigest
import Stream._
import akka.actor.ActorSystem



abstract class Info
case class ChunkInfo(nBytes:Long,offset:Long,digest:String) extends Info {
  val _1 = nBytes
  val _2 = offset
  val _3 = digest
}
case class AccInfo(msg:String) extends Info
case class ReqInfo(resource:String,offset:Long) extends Info
case class FileInfo(nBytes:Long,ChunkSize:Int,offset:Long,signature:String) extends Info {
  val _1 = nBytes
  val _2 = ChunkSize
  val _3 = offset
  val _4 = signature
}

object Chunks {
  val md = MessageDigest.getInstance("SHA")

  
  type Chunk = (Array[Byte],Long,String)//Data,offset,digest
  //type ChunkInfo = (Long,Long,String)//nBytes,offset,digest
  val CHUNK_INF = ":ci(.*):(.*):(.*)".r//nBytes,offset,digest
  
  //type FileInfo = (Long,Int,Long,String)//nBytes,ChunkSize,offset,signature
  val FILE_INF = ":fLi(.*):(.*):(.*):(.*)".r//nBytes,ChunkSize,offset,signature
  val REQ_INF = ":fGe(.*):(.*)".r
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
  
  def getChunkInfo(ch:Chunk):Info = {
    val nBytes = ch._1.length
    val offset = ch._2
    val digest = ch._3
    return ChunkInfo(nBytes,offset,digest)
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
    println("Ack sent")
  }
  def getAcc(is:InputStream,timeout:Int=10):AccInfo = {
    val in = new BufferedReader(new InputStreamReader(is))
    val future = Future {
      in.readLine()
    }
    val accTxt = Await.result(future, timeout second).asInstanceOf[String]
    accTxt match {
      case ACC(msg) => {
        return AccInfo(msg)
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
  
  def sendReqInfo(resource:String,os:OutputStream, offset:Long=0)={
    val out = new PrintWriter(os, true)
    out.println(s":fGe$resource:$offset")
  }
  
  def getReqInfo(is:InputStream):ReqInfo = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case REQ_INF(resource,offset) => {
        return ReqInfo(resource,offset.toLong)
      }
      case _ =>{
        throw new IOException("Unrecognized File Info Format")
      }
    }
  }
  
  
  def getFileInfo(is:InputStream):Info = {
    val in = new BufferedReader(new InputStreamReader(is))
    in.readLine() match {
      case FILE_INF(nBytes,chunkSize,offset,signature) => {
        return FileInfo(nBytes.toLong,chunkSize.toInt,offset.toLong,signature)
      }
      case _ =>{
        throw new IOException("Unrecognized File Info Format")
      }
    }
  }
  
  def sendFile(os:OutputStream,f:RandomAccessFile,offset:Long):Boolean = {
    f.seek(offset)
    val fin = new FileInputStream(f.getFD())
    try{
    wireCodes.copy(fin, os)
    }
    catch{
      case s:SocketException => return false
    }
    return true
  }
  
  def getFile(is:InputStream,f:RandomAccessFile,offset:Long,fileSize:Long,bufSize:Int=2048):Long = {
    f.seek(offset)
    val fout = new FileOutputStream(f.getFD())
    wireCodes.copy(is,fout)
//    val fp = f.getFilePointer()
//    println(s"offset set at: $fp, arg received: offset")
//    var gotten:Long = 0
//    var len:Int = 0
//    val buf = new Array[Byte](bufSize)
//    try{
//    len = is.read(buf)
//    val nBytesToGet = fileSize - offset
//    println(s"File size: $fileSize, offset: $offset")
//    val bytePercent:Long = nBytesToGet/100
//    var nextPercent:Long = bytePercent // add 1% of the bytes to the offset
//    while(offset+gotten != fileSize && len != -1) {
//      f.write(buf,0,len)
//      gotten+=len
//      //if(gotten > nextPercent){
//        //print("=")
//        //nextPercent+=bytePercent
//        var bytesLeft = fileSize - offset - gotten
//        //println(s"bytes left = $bytesLeft")
//      //}
//      len=is.read(buf)
//    }
//    }
//    catch{
//      case s:SocketException => return offset+gotten//incomplete
//    }
//    f.close()
//    return -1//Success
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
        return ChunkInfo(nBytes.toLong,offset.toLong,digest)
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

class FileTransfer(s:Socket) {
  val in = s.getInputStream()
  val out = s.getOutputStream()
  println("FileTransfer object created")
  def get(localResource:String,remoteResource:String,offset:Long=0):Long = {
    val f = new RandomAccessFile(localResource,"rw")
    //Chunks.sendReqInfo(remoteResource,out,offset)//Send the file info to the server (it only cares about offset)
    val acc = Chunks.getAcc(in)//Get ack of file info received
    acc.msg match {
      case ":Err404" => throw new IOException("File not Found")
      case _ =>
    }
    val size = acc.msg.toLong
    Chunks.sendAcc("SND",out)//Tell server to start sending
    val newOffset = Chunks.getFile(in,f,offset,size)
    if(newOffset == -1)
      println("File received successfully")
    else
      println(s"File reception incomplete, new offset is $newOffset")
    return newOffset
  }
  
  def send(localResource:String,offset:Long):Boolean = {
    val fle = new File(localResource)
    if(!fle.exists()){
      Chunks.sendAcc(":Err404",out)
      return false
    }
    val f = new RandomAccessFile(localResource,"rw")
    //val inf = Chunks.getReqInfo(in)
    val size = f.length()
    Chunks.sendAcc(s"$size",out)
    Chunks.getAcc(in)//Get snd acc
    Thread.sleep(10)
    val res =Chunks.sendFile(out,f,offset)
    close()
    return res
  }
  
  def close(){
    in.close()
    out.close()
    s.close()
  }
}


    



