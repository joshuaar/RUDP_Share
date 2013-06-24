package share.vid
import uk.co.caprica.vlcj.player._
import java.lang.ProcessBuilder

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
    return OS.indexOf("nix") >= 0
  }
 
}

class RTPStream {
  val mp = new MediaPlayerFactory()//not used right now
  def serve(resource:String,remoteAddr:String,remotePort:Int,startTime:Long=0,kBRate:Int=700):Process = {
    
    val streamProp = s"'#transcode{vcodec=h264,vb=$kBRate,scale=1,acodec=mpga,ab=128,channels=2,samplerate=44100}:rtp{mux=ts,dst=$remoteAddr,port=$remotePort}'"
    
    if(osDetect.isUnix()) {
      val proc = new ProcessBuilder("cvlc",resource,"--start-time",startTime.toString,
          "--sout",streamProp)
      val start = proc.start()
      start
    }
    
    else{
      println("Only POSTIX compliant OS is supported")
      exit(1)
    }
    
  }
}