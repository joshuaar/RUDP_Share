package share.protocol.rudp
import share.msg._
//import net.rudp._
import java.net._
import akka.actor._
import java.io._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import udt._
import ExecutionContext.Implicits.global
import java.nio.ByteBuffer
import share.sync._

case class ParseException(msg:String) extends Exception

trait reqFactory {
  def fromString(req:String):Request
  val matcher:scala.util.matching.Regex
}

abstract class Request {
  def toString():String
  
}
object getReq extends reqFactory{
  val matcher = "GeT:(.*):(.*)".r//resource, offset
  def fromString(req:String):getReq = {
    req match {
      case matcher(res,off) => return getReq(res,off.toLong) 
      case s:String => throw new ParseException("Unexpected format for get request: "+ s)
      case _ => throw new ParseException("Unknown error")
    }
  }
}
case class getReq(res:String,off:Long) extends Request {
  override def toString():String = {
    return s"GeT:$res:$off"
  }
}

object ack extends reqFactory {
  val matcher = "AcK:(.*)".r
  def fromString(req:String):ack = {
    req match {
      case matcher(res) => return ack(res) 
      case s:String => throw new ParseException("Unexpected format for get request: "+ s)
      case _ => throw new ParseException("Unknown error")
    }
  }
}

case class any(msg:String) extends Request {
  override def toString():String = {
    return msg
  }
}

object any extends reqFactory {
  val matcher = "(.*)".r
  def fromString(req:String):any = {
    req match {
      case matcher(res) => return any(res) 
      case s:String => throw new ParseException("Unexpected format for get request: "+ s)
      case _ => throw new ParseException("Unknown error")
    }
  }
}

case class ack(msg:String) extends Request {
  override def toString():String = {
    return s"AcK:$msg"
  }
}

case class ftReq extends Request {
  override def toString():String = {
    "fTR:"
  }
}

object ftReq extends reqFactory {
  val matcher = "fTR:".r
  def fromString(req:String):ftReq = {
    req match {
      case `matcher` => return ftReq()
      case s:String => throw new ParseException("Unexpected format for get request: "+ s)
      case _ => throw new ParseException("Unknown error")
    }
  }
}

object Server {
  def makeServer(localPort:Int, remoteHost:String,remotePort:Int):UDTSocket = {
    puncher.punch(remoteHost, localPort, remotePort) //Punch a hole
    val serverSocket = new UDTServerSocket(InetAddress.getLocalHost(),localPort:Int)
    val future = Future {
      serverSocket.accept()
    }
    Await.result(future,60 second).asInstanceOf[UDTSocket]
  }
}
  
object Client {
  def makeClient(localPort:Int, remoteHost:String,remotePort:Int):UDTClient = {
    //puncher.punch(remoteHost, localPort, remotePort) //Punch a hole
    val cli = new UDTClient(InetAddress.getLocalHost(),localPort)
    val future = Future {
      cli.connect(remoteHost,remotePort)
    }
    Await.result(future,60 second).asInstanceOf[Unit]
    cli
  }
 
  
  def hex2bytes(hex: String): Array[Byte] = {
   hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
   }
  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
   sep match {
     case None => bytes.map("%02x".format(_)).mkString
     case _ => bytes.map("%02x".format(_)).mkString(sep.get)
     }
   }
 
  
 def expect(is:InputStream,reqType:reqFactory,timeout:Int=10):Request = {
   val future = Future{
   val buf = new Array[Byte](4)
   while(is.read(buf)==0){
     //println("Read nothing first")
     Thread.sleep(100)
   }
   println("Read something")
   val bb = ByteBuffer.wrap(buf)
   val len = bb.getInt()
   //val cmdBuf = new Array[Byte](len)
   //bb.get(cmdBuf)
   //println(cmdBuf)
   println(s"Bytes to read: $len")
   val reqStream = new ByteArrayOutputStream()
   var bytesRead = 0
   val buf2 = new Array[Byte](2048)
   var nRead = is.read(buf2)
   while(nRead == 0){
     println("Read nothing second")
     Thread.sleep(100)
     nRead = is.read(buf2)
   }
   while(bytesRead < len){
     reqStream.write(buf2,0,nRead)
     println(s"Read: $bytesRead length:$len")
     bytesRead+=nRead
     nRead=is.read(buf2)
   }
   reqStream.toString("UTF-8")
   }
   val req = Await.result(future,timeout second).asInstanceOf[String]
   return reqType.fromString(req)
 }
 
 def expectObj[X<:Byteable](is:InputStream,timeout:Int=10):X = {
   val future = Future{
   val buf = new Array[Byte](4)
   while(is.read(buf)==0){
     //println("Read nothing first")
     Thread.sleep(100)
   }
   println("Read something")
   val bb = ByteBuffer.wrap(buf)
   val len = bb.getInt()
   println(s"Bytes to read: $len")
   val reqStream = new ByteArrayOutputStream()
   var bytesRead = 0
   val buf2 = new Array[Byte](2048)
   var nRead = is.read(buf2)
   while(nRead == 0){
     println("Read nothing second")
     Thread.sleep(100)
     nRead = is.read(buf2)
   }
   while(bytesRead < len){
     reqStream.write(buf2,0,nRead)
     println(s"Read: $bytesRead length:$len")
     bytesRead+=nRead
     nRead=is.read(buf2)
   }
   reqStream.toByteArray()
   }
   val req = Await.result(future,timeout second).asInstanceOf[Array[Byte]]
   return ByteableFactory.fromByteArray[X](req)
 }
 
 def writeInt(i:Int,os:OutputStream) = {
   val bb = ByteBuffer.allocate(4)
   bb.putInt(i)
   bb.flip()
   os.write(bb.array())
 }
 def writeLong(i:Long,os:OutputStream) = {
   val bb = ByteBuffer.allocate(8)
   bb.putLong(i)
   bb.flip()
   os.write(bb.array())
 }
 def writeString(s:String, os:OutputStream) = {
   println(s"Writing string $s to the wire")
   val bytes = s.getBytes("UTF-8")
   val len = bytes.length
   writeInt(len,os)
   os.write(bytes)
 }
 def writeBytes(b:Array[Byte],os:OutputStream) = {
   println(s"Writing byte array to the wire")
   val len = b.length
   writeInt(len,os)
   os.write(b)
 }
 
 def copy(in:InputStream, out:OutputStream,nBytes:Long):Long = {
        val buf = new Array[Byte](8192)
        var len = 0;
        len = in.read(buf)
        while(len == 0){
          Thread.sleep(100)
          len = in.read(buf)
        }
        var written:Long = 0
        try{
        while (written < nBytes && len != -1) {
            out.write(buf, 0, len)
            written+=len
            //println(s"Sent $len bytes")
            len = in.read(buf)

        }
        }
        catch{
          case s:SocketException => return written
        }
        println("Stream copied")
        return -1
    }
}


class Client(localPort:Int, remoteHost:String,remotePort:Int) {
  val udt = Client.makeClient(localPort,remoteHost,remotePort)
  val os = udt.getOutputStream()
  val is = udt.getInputStream()
  
//  def getObject(objType:ByteableFactory):Byteable = {
//    Client.writeString(req.toString,os)
//    val response = Client.expect(is,ack).asInstanceOf[ack]
//    val nBytes = response.msg.toLong
//    Client.writeString(ack(response.msg).toString(),os)
//    val bos = new ByteArrayOutputStream()
//    Client.copy(is,bos,nBytes)
//    FileTree.fromByteArray(bos.toByteArray())
//  }
  
  def getShares(req:ftReq):ShareContainer = {
    Client.writeString(req.toString(),os)
    Client.expect(is,ShareContainer).asInstanceOf[ShareContainer]
  }
  
  def getFile(req:getReq,dest:String):Long = {
    Client.writeString(req.toString(),os)
    val response = Client.expect(is,ack).asInstanceOf[ack]
    val nBytes = response.msg.toLong
    
    val fileOut = new RandomAccessFile(dest,"rw")
    fileOut.seek(req.off)
    val fos = new FileOutputStream(fileOut.getFD())
    Client.writeString(ack(response.msg).toString(),os) //tell host to send
    Client.copy(is,fos,nBytes)
  }
}

class Server(localPort:Int,remoteHost:String,remotePort:Int) {
  val udt = Server.makeServer(localPort,remoteHost,remotePort)
  val os = udt.getOutputStream()
  val is = udt.getInputStream()
  
  def listen():Request = {
    val req = Client.expect(is,any,60).asInstanceOf[any].msg
    req match {
      case getReq.matcher(a,b) => {
        getReq.fromString(req)
      }
      case ack.matcher(a) => {
        ack.fromString(req)
      }
      case s:String => {
        any.fromString(s)
      }
      case _ => {
        throw new Exception("Unknown error")
      }
    }
  }
  
  def sendShares(s:ShareContainer) = {
    Client.writeString(s.toString(),os)
  }
  
  def sendFile(req:getReq) = {
    val f = new RandomAccessFile(req.res,"rw")
    f.seek(req.off)
    val nBytes = f.length - req.off
    Client.writeString(ack(nBytes.toString()).toString(),os) //send # bytes to client
    val a = Client.expect(is, ack)
    val fis = new FileInputStream(f.getFD())
    Client.copy(fis,os,nBytes)
  }
}
  

  
  


