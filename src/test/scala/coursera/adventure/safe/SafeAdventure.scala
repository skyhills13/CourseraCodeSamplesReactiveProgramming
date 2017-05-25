package coursera.adventure.safe

import coursera.adventure._
import scala.util.{Failure, Success, Try}

object Adventure {
  def apply(): Adventure = new Adventure(){
    var eatenByMonster: Boolean = true
    val treasureCost: Int = 42
  }
}

trait Adventure {

  var eatenByMonster: Boolean
  val treasureCost: Int

  def collectCoins(): Try[List[Coin]] = ???

  def buyTreasure(coins: List[Coin]): Try[Treasure] = ???

  //그래서 Try[T]라는 모나드로 값을 감싸서 exception이 날 수도 있음을 명시적으로 알려주기로 함.
  //그래서 만들었는데.. 좀..안예뻐. 이유가 무엇인고하니, 케이스 처리를 하면서 신경써야함.
  def PlayI(): Unit = {
    val adventure = Adventure()
    val coins: Try[List[Coin]] = adventure.collectCoins()
    val treasure: Try[Treasure] = coins match {
      case Success(cs)          => adventure.buyTreasure(cs)
      case Failure(t)           => Failure(t)
    }
  }
  //그럴때 등장하는 high order ftn flatMap으로 깔끔하게 처리
  def PlayII(): Unit = {
    val adventure = Adventure()
    val coins: Try[List[Coin]] = adventure.collectCoins()
    val treasure: Try[Treasure] = coins.flatMap(cs => adventure.buyTreasure(cs))
  }

  //for로도 표현할 수 있음
  def PlayIII(): Unit = {
    val adventure = Adventure()
    val treasure: Try[Treasure] = for {
      coins <- adventure.collectCoins()
      treasure <- buyTreasure(coins)
    } yield treasure
  }

  //  trait Try[T] {
//    def map[S](op: T => S): Try[S] = this match {
//      case Success(value) => Try(op(value))
//      case failure@Failure(t) => failure
//    }
//  }
//  object Try {
//    def apply[T](expr: => T): Try[T] = {
//      try Success(expr)
//      catch { case t => Failure(t) }
//    }
//  }
}
