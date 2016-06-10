package controllers

import java.net.{SocketAddress, InetAddress, DatagramPacket, DatagramSocket}
import javax.inject._
import play.api._
import play.api.mvc._
import utils.PacketUtils._

@Singleton
class ColorController @Inject() extends Controller {

  val hue = 20
  val saturation = 90
  val brightness = 10
  val kelvins = 3000
  val transitionTime = 2000

  def index = Action {
    Ok(views.html.index())
  }

  def setColor = Action {
    val normalizedHue = hue / 360.0 * 65535
    val normalizedSaturation = saturation / 100.0 * 65535
    val normalizedBrightness = brightness / 100.0 * 65535

    // Look for a bulb, then set its color
    getIP match {
      case Some(ip) =>
        val colorPayload = getLittleEndianBytes(normalizedHue) ++
          getLittleEndianBytes(normalizedSaturation) ++
          getLittleEndianBytes(normalizedBrightness) ++
          getLittleEndianBytes(kelvins) ++
          getLittleEndianBytes(transitionTime) ++
          Array[Byte](0x00, 0x00)
        val buf: Array[Byte] = makePacket(MESSAGE_SETCOLOR, colorPayload)

        val inet = InetAddress.getByName(ip)
        val bufferSize = buf.length

        val packet = new DatagramPacket(buf, bufferSize, inet, PORT)
        val sock = new DatagramSocket(PORT)
        sock.send(packet)
        sock.close()
        Ok(s"hsb: ${normalizedHue}, ${normalizedSaturation}, ${normalizedBrightness}")
      case None =>
        Ok("I couldn't find a bulb on the network")
    }
  }

}
