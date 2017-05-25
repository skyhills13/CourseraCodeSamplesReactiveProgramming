package coursera.socket.unsafe

object Socket {
  def apply(): Socket = new Socket(){
  }
}

trait Socket {
  // 이번엔 또 다른 효과인 latency에 대해 이야기 함.
  // 계산하는데에 실패(exception)할 수도 있고 시간이 걸릴 수(latency)도 있으니까, 이걸 비동기로 받을 수 있는 새로운 모나드를 도입
  def readFromMemory(): Array[Byte] = ???
  def sendToEurope(packet: Array[Byte]): Array[Byte] = ???

  // 여기서 얼마까지 blocking 될 수 있는지 설명한다고 에릭 마이어 아저씨가
  // 시간을 사람 시간을 바꿔서 보여주고 깨알..
  def sendPacketToEuropeAndBack(): Unit = {
    val socket = Socket()
    val packet = socket.readFromMemory()
    val confirmation = socket.sendToEurope(packet)
  }
}
