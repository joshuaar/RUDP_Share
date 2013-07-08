package share.protocol.rudp
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.SocketException
import java.net.InetAddress

/**
* Simple class for sending and receiving UDP packets
* pt: Port number, nBytes: Size of packets to send
*/
object puncher {
  /**
* Sends a packet to a remote host
*/
  def send(data:Array[Byte],IP:InetAddress,port:Int,sock:DatagramSocket){
    val toSend = new DatagramPacket(data,data.length,IP,port)
    sock.send(toSend)
  }
  
  def punch(host:String,localPort:Int,remotePort:Int,retransmits:Int=10) = {
    val sock = new DatagramSocket(localPort)
    val rh = InetAddress.getByName(host)
    for(i <- List.range(0,retransmits)) {
      send("PNCH".getBytes(),rh,remotePort,sock)
    }
    sock.close()
  }
}
