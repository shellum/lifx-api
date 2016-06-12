package controllers

import java.net.{DatagramPacket, DatagramSocket, InetAddress, SocketAddress}
import javax.inject._

import play.api._
import play.api.mvc._
import utils.PacketUtils
import utils.PacketUtils._

@Singleton
class ColorController @Inject() extends Controller {

  val saturation = 90
  val brightness = 10
  val kelvins = 3000
  val transitionTime = 50

  def index = Action {
    Ok(views.html.index())
  }

  def setColor(hue: Int) = Action {
    val validatedHue = hue match {
      case x if x<=0 => 1
      case x if x>=360 => 359
      case _ => hue
    }
    setBulb(validatedHue, saturation, brightness) match {
      case Some(ip) => Ok(s"hsb: ${validatedHue}, ${saturation}, ${brightness} for ${ip}")
      case None => Ok("I couldn't find a bulb on the network")
    }
  }

  def fade(brightness: Int) = Action {
    val validatedBrightness: Int = brightness match {
      case x if x<=0 => 1
      case x if x>=50 =>50
      case _ => brightness
    }
    getHSB() match {
      case Some((hue, saturation, _)) => setBulb(hue, saturation, validatedBrightness) match {
        case Some(ip) => Ok(s"hsb: ${hue}, ${saturation}, ${brightness} for ${ip}")
        case None => Ok("I couldn't find a bulb on the network")
      }
      case None => Ok("no")
    }
  }

  def getBulb() = Action {
    PacketUtils.getHSB() match {
      case Some(t) => Ok(s"hsb:${t._1} ${t._2} ${t._3}")
      case None => Ok("no")
    }
  }

  def setBulb(hue: Int, saturation: Int, brightness: Int): Option[String] = {
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
        Some(ip)
      case _ =>
        None
    }
  }
}
