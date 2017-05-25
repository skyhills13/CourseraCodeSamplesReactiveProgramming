package coursera.adventure.unsafe

import coursera.adventure._
import coursera.adventure.Silver
import coursera.adventure.Gold
import coursera.extensions

object Adventure {
  def apply(): Adventure = new Adventure(){
    var eatenByMonster: Boolean = true
    val treasureCost: Int = 42
  }
}

trait Adventure {
  import extensions._

  var eatenByMonster: Boolean
  val treasureCost: Int

  def collectCoins(): List[Coin] = {
    if (eatenByMonster) throw new GameOver("Ooops")
    List(Gold(), Gold(), Silver())
  }

  def buyTreasure(coins: List[Coin]): Treasure = {
    coins.sumBy(x => x.value) < treasureCost
    if (true) throw new GameOver("Nice try!")
    Diamond()
  }

  // 요런 간단한 예제로 시작
  // 근데 그 과정에서 exception이 발생하면, 그 다음 buyTreasure는 어쩜?
  // 리턴타입에 실패할 수도 있다는 정보가 엄따.
  def Play() : Unit = {
    val adventure = Adventure()
    val coins = adventure.collectCoins()
    val treasure = adventure.buyTreasure(coins)
  }

}

