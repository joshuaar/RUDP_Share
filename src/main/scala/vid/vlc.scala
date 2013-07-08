package share.protocol.vid
import java.lang.ProcessBuilder
import java.io.IOException

object osDetect {
  val OS = System.getProperty("os.name").toLowerCase
  
  val vlcFolder64 = "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe"
  val vlcFolder32 = "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe"
  
  def isWindows():Boolean ={
    return OS.indexOf("win") >= 0
  }
  
  def isMac():Boolean ={
    return OS.indexOf("mac") >= 0
  }
  
  def isUnix():Boolean ={
    return OS.indexOf("nux") >= 0
  }
 
}

class RTPStream {
  //val mp = new MediaPlayerFactory()//not used right now
  def serve(resource:String,remoteAddr:String,remotePort:Int,startTime:Long=0,kBRate:Int=700):Process = {
    val cmd = "cvlc /home/josh/testvid.avi --start-time 121 --sout '#transcode{vcodec=h264,vb=800,scale=1,acodec=mpga,ab=128,channels=2,samplerate=44100}:rtp{mux=ts,dst=192.168.1.141,port=5004}'"

    if(osDetect.isUnix()) {
      val proc = Runtime.getRuntime().exec("vlc rtp://@:5004")
      proc
    }
    
    else{
      throw new IOException("OS Not supported")
    }
    
  }
}

object streamTest {//extends App {
  val stream = new RTPStream()
    val inst = stream.serve("/home/josh/testvid.avi","127.0.0.1",5005)//serve(resource:String,remoteAddr:String,remotePort:Int,startTime:Long=0,kBRate:Int=700)
    val is = inst.getErrorStream()
    val buf = new Array[Byte](2048)
    var count = 0
    while(count <=100) {
      println("reading")
      val len = is.read(buf)
      if(len>0)
        println(new String(buf))
      Thread.sleep(50)
      count+=1
    }
}
