package controllers

import java.net.{InetAddress, DatagramPacket, DatagramSocket}
import javax.inject._
import play.api._
import play.api.mvc._

@Singleton
class ColorController @Inject() extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def setColor = Action {

    val ip = "192.168.1.11"
    val port = 56700
    val hue = 70 / 360.0 * 65535
    val sat = 90 / 100.0 * 65535
    val bri = 10 / 100.0 * 65535
    val kel = 3000
    val transitionTime = 2000
    // LIFX LAN Protocol Structure & Explination (all little endian)
    val buf: Array[Byte] = Array[Byte](
      // Frame Starts Here
      0x31, 0x00, // Size of entire message
      0x00, 0x34, // Origin(0), Tagged(0 for non discovery), Addressable(0), & Protocol (1024) bits
      0x00, 0x00, 0x00, 0x00, // Source ID (decided by client, used in responses)

      // Frame Address Starts Here
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // MAC Addr or 0 for all devices
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Reserved
      0x00, 0x00, // Reserved(0), Ack(bool), Resp(bool), Sequence # (Client specified/0)

      // Protocol Header Starts Here
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Reserved (0)
      0x66, 0x00, // Message Type (set color==103)
      0x00, 0x00, // Reserved

      // Message starts here
      0x00, // Reserved
      ((hue.toInt) & 255).toByte,
      ((hue.toInt >> 8) & 255).toByte,
      ((sat.toInt) & 255).toByte,
      ((sat.toInt >> 8) & 255).toByte,
      ((bri.toInt) & 255).toByte,
      ((bri.toInt >> 8) & 255).toByte,
      ((kel.toInt) & 255).toByte,
      ((kel.toInt >> 8) & 255).toByte,
      ((transitionTime.toInt) & 255).toByte,
      ((transitionTime.toInt >> 8) & 255).toByte,
      0x00, 0x00 // Transision time (ms)
    )

    val inet = InetAddress.getByName(ip)
    val bufferSize = buf.length

    val packet = new DatagramPacket(buf, bufferSize, inet, port)
    val sock = new DatagramSocket(port)
    sock.send(packet)
    sock.close()
    Ok(s"hsb: ${hue}, ${sat}, ${bri}")
  }

}
