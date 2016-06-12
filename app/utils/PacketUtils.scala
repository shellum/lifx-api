package utils

import java.net.{DatagramSocket, DatagramPacket, InetAddress}

object PacketUtils {

  val PORT = 56700
  val MESSAGE_GETSTATUS = 2.toByte
  val MESSAGE_SETCOLOR = 102.toByte
  val MESSAGE_GET = 101.toByte
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
    sock.setSoTimeout(SOCKET_TIMEOUT)
    // Get at least two responses. One may be us.
    val addresses = (1 to 2).toList.map(_ => {
      try {
        val responsePacket = getResponse(ip, sock)
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

  def getResponse(ip: String, sock: DatagramSocket): DatagramPacket = {
    val queryPayload = Array[Byte]()
    val buf: Array[Byte] = makePacket(MESSAGE_GETSTATUS, queryPayload)

    val inet = InetAddress.getByName(ip)
    val bufferSize = buf.length

    val responsePacket = new DatagramPacket(Array[Byte](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 50)
    sock.setSoTimeout(SOCKET_TIMEOUT)
    sock.receive(responsePacket)
    println(s"got packet size ${responsePacket.getData()(0)}")
    responsePacket
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

  def getValueFromLittleEndianBytes(byte1: Byte, byte2: Byte): Int = {
    // Silly signed data types... forget all that and use two's compliment conversion
    var b1 = byte1.toInt
    var b2 = byte2.toInt
    if (b1<0){b1=0-b1-1^0xff}
    if (b2<0){b2=0-b2-1^0xff}
    0 + b1 + (b2 << 8)
  }

  def getHSB(): Option[(Int,Int,Int)] = {
    val buf: Array[Byte] = makePacket(MESSAGE_GET, Array[Byte]())

    // TODO: Fix bug in getting bad packet the 2nd+ time after getIP call
    //getIP match {
    //  case Some(ip) =>
    val ip="192.168.1.7"
        println(s"IPPP: ${ip}")
        val inet = InetAddress.getByName(ip)
        val bufferSize = buf.length

        val packet = new DatagramPacket(buf, bufferSize, inet, PORT)
        val sock = new DatagramSocket(PORT)
        sock.send(packet)
        val response = getResponse(ip, sock)
        val res: Array[Byte] = response.getData()

        val nonNormalizedHue = getValueFromLittleEndianBytes(res(36), res(37))
        val normalizedHue = nonNormalizedHue * 360 / 65535
        val nonNormalizedSaturation = getValueFromLittleEndianBytes(res(38), res(39))
        val normalizedSaturation = nonNormalizedSaturation * 100 / 65535
        val nonNormalizedBrightness = getValueFromLittleEndianBytes(res(40), res(41))
        val normalizedBrightness = nonNormalizedBrightness * 100 / 65535
        sock.close()
        Some((normalizedHue, normalizedSaturation, normalizedBrightness))
    //  case _ =>
    //    None
    //}
  }

}
