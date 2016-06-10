package utils

import java.net.{DatagramSocket, DatagramPacket, InetAddress}

object PacketUtils {

  val PORT = 56700
  val MESSAGE_GETSTATUS = 2.toByte
  val MESSAGE_SETCOLOR = 102.toByte
  val SOCKET_TIMEOUT = 1000

  def getIP = {
    val ip = "255.255.255.255"
    val queryPayload = Array[Byte]()
    val buf: Array[Byte] = makePacket(MESSAGE_GETSTATUS, queryPayload)

    val inet = InetAddress.getByName(ip)
    val bufferSize = buf.length

    val packet = new DatagramPacket(buf, bufferSize, inet, PORT)
    val sock = new DatagramSocket(PORT)
    sock.send(packet)
    val responsePacket = new DatagramPacket(Array[Byte](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 50)
    sock.setSoTimeout(SOCKET_TIMEOUT)
    // Get at least two responses. One may be us.
    val addresses = (1 to 2).toList.map(_ => {
      try {
        sock.receive(responsePacket)
        Some(responsePacket.getAddress.getHostAddress)
      } catch {
        case e: Throwable => None
      }
    })
    val addressSet = addresses.toSet[Option[String]]
    val filteredAddresses = addressSet.filter(a => a != None && a.get != InetAddress.getLocalHost.getHostAddress)
    sock.close()
    filteredAddresses.last
  }

  def makePacket(messageType: Byte, messagePayload: Array[Byte]) = {
    // LIFX LAN Protocol Structure & Explination (all little endian)
    Array[Byte](
      // Frame Starts Here
      0x31, 0x00, // Size of entire message
      0x00, 0x34, // Origin(0), Tagged(0 for non discovery), Addressable(0), & Protocol (1024) bits
      0x00, 0x00, 0x00, 0x00, // Source ID (decided by client, used in responses)
      // Frame Address Starts Here
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // MAC Addr or 0 for all devices
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Reserved
      0x00, 0xff.toByte, // Reserved(0), Ack(bool), Resp(bool), Sequence # (Client specified/0)
      // Protocol Header Starts Here
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Reserved (0)
      messageType, 0x00, // Message Type (set color==103)
      0x00, 0x00, // Reserved
      // Message starts here
      0x00) ++ // Reserved
      messagePayload
  }

  def getLittleEndianBytes(input: Double): Array[Byte] = {
    Array[Byte](
      ((input.toInt) & 255).toByte,
      ((input.toInt >> 8) & 255).toByte
    )
  }
}
