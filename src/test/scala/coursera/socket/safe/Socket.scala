package coursera.socket.safe

import scala.util.{Failure, Success}
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.collection.immutable.Queue
import akka.serialization._
import coursera.socket.EmailMessage

object Socket {
  def apply(): Socket = new Socket(){}
}

trait Socket {

  val serialization: Serialization = ???

  val memory = Queue[EmailMessage](
    EmailMessage(from = "Erik", to = "Roland"),
    EmailMessage(from = "Martin", to = "Erik"),
    EmailMessage(from = "Roland", to = "Martin")
  )

  def readFromMemory(): Future[Array[Byte]] = {
    Future {
      val email = memory.dequeue
      val serializer = serialization.findSerializerFor(email)
      serializer.toBinary(email)
    }
  }

  def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] = {
    Http("mail.server.eu", Request(packet)).filter(_.isOK).map(_.body)
  }

  def sendTo(url: String, packet: Array[Byte]): Future[Array[Byte]] =
    Http(url, Request(packet)).filter(_.isOK).map(_.body)


  /*
{
    trait Socket {
        def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]] =
            Http(url, Request(packet))
                .filter(response => response.isOK)
                .map(response => response.toByteArray)
        def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] =
            sendTo(mailServer.europe, packet)
                .recoverWith {
                    case europeError => sendTo(mailServer.usa, packet) //partialFunction이기 때문에 case _ => () 가 없어도 되는거야. 모든 case에 대한 처리를 안해도 된다고.
                        .recover {
                            case usaError => usaError.getMessage.toByteArray
                        }
                }
    }
}
  * */
// 그나마 이건 for로 바꿔서 나아 보이지만 로직이 뭔가 이상
  def sendToSafeI(packet: Array[Byte]): Future[Array[Byte]] =
    sendTo("...europe...", packet) recoverWith {
      //유럽 보내다 에러나면 미국으로 보내
      case europeError => sendTo("...usa...", packet) recover {
        //미국 보내다 에러나면 미국에서 에러메시지 받아
        case usaError  => usaError.getMessage.getBytes
      }
    }
// 그래서 또 다른걸 새로 도입함 fallbackTo
  // 이게 뭐냐면, this future가 실패하면 that future의 결과를, that future도 실패하면 this future의 error message를 반환하는 것
  //이제 안이상함
  //그러니까 이게 뭐냐면, async가 가능하면 그렇게 하고, blocking이 필요하면 하도록 한 것
  /*
  def fallbackTo(that: =>Future[T]): Future[T] = { this recoverWith {
    case _ => that recoverWith { case _ => this }
    }
  }
  * */
  def sendToSafeII(packet: Array[Byte]): Future[Array[Byte]] =
    sendTo("...europe...", packet) fallbackTo { sendTo("...usa...", packet) } recover {
      case europeError => europeError.getMessage.getBytes
    }

  //퓨처 모나드를 도입하고
  //비동기 호출을 하고 callback 붙여놓고 다른일을 할 수 있도록 함.
  //onComplete의 파람 callback은 패턴 매칭이 필요함(success, fail)
  def sendPacketToEuropeAndBackI(): Unit = {

    val socket = Socket()

    val packet: Future[Array[Byte]] = socket.readFromMemory()

    val confirmation: Unit =
      packet onComplete {
        case Success(p) => socket.sendToEurope(p)
        case Failure(t) => ???
      }

  }
  //퓨처 구현 방식 두가지
//  trait Future[T] {
//    def onComplete(callback: Try[T] => Unit) //Try로 한번에 담아서
//                  (implicit executor: ExecutionContext): Unit
//  }

//  trait Observer[T] {
//    def onNext(value: T): Unit
//    def onError(err: Throwable): Unit
//  }
//
//  trait Future[T] {
//    def onComplete(success: T => Unit, failed: Throwable => Unit): Unit //Try가 아니다!! 대신 두개로 나눴지
//    def onComplete(callback: Observer[T]): Unit
//  }

  //Unit이 리턴타입이라 문제임. 이래저래 confirmation을 안으로 넣어도 보고 하는데.. 여긴 뭐가 문제일까?
  def sendPacketToEuropeAndBackII(): Unit = {
    val socket = Socket()

    val packet: Future[Array[Byte]] =
      socket.readFromMemory()

    packet onComplete {
      case Success(p) => {
        val confirmation: Future[Array[Byte]] = socket.sendToEurope(p)

      }
      case Failure(t) => ???
    }
  }

// 콤비네이터 도입!
//  trait Future[T] extends Awaitable[T] {
//  def filter(p: T => Boolean): Future[T]
//
//  def flatMap[S](func: T => Future[S]): Future[S]
//
//  def map[S](func: T => S): Future[S]
//}

// flatMap과 비슷한데 에러 핸들링을 하고
//    def recoverWith(func: PartialFunction[Throwable, Future[T]]): Future[T] //어떤것은 처리할 수 있고, 어떤 것은 처리할 수 없는 func - partialFunction (isDefinedAt으로 확인으 랳야해)
// map과 비슷한데 에러 핸들링을 하고
//    def recover(func: PartialFunction[Throwable, T]): Future[T]
//  }

  //나름 괜찮아 진것 같아 보여. 하지만 에러 핸들링은?!
  def sendPacketToEuropeAndBackIII(): Unit = {
    val socket = Socket()
    val packet: Future[Array[Byte]] = socket.readFromMemory()
    val confirmation: Future[Array[Byte]] = packet.flatMap(socket.sendToEurope(_))
  }

  //그래서 아까 새로 나온 특별한 것을 쓴다! 위에 설명이 있다.
  def sendPacketToEuropeAndBackIV(): Unit = {
    val socket = Socket()
    val confirmation: Future[Array[Byte]] = for {
      packet       <- socket.readFromMemory()
      confirmation <- socket.sendToSafeII(packet)
    } yield confirmation
  }

  def sendToAndBackUp(packet: Array[Byte]):Future[(Array[Byte], Array[Byte])] = {
    val europeConfirm = sendTo("...europe...", packet)
    val usaConfirm = sendTo("...usa...", packet)
    europeConfirm.zip(usaConfirm)
  }

//  이제 Awaitable //자바 퓨쳐에도 비슷한게 있지. get()
//    trait Awaitable[T] extends AnyRef {
//      abstract def ready(atMost: Duration): Unit
//      abstract def result(atMost: Duration): T
//    }
//  어싱크 파이프라인에서 블락 펑션 절대 쓰지 말라함. 막 블러디 머더라고함 ㅋㅋㅋㅋ 토비님이 캠프에서 말씀하신거랑 비슷한 맥락.
  //꼭 필요할 때만 쓰라고 함

  //그 다음으로는, recursion에 대한 이야기
  /*
  {
    trait Future[T] {

        def retry(nTimes: Int)(block: => Future[T]): Future[T] = {
            // retry block at most nTimes
            // and give up after that
            if(nTimes <= 0) {
                Future.failed(new Exception("can't do"))
            } else {
                block fallbackTo { retry(nTimes-1){ block } }
              }
          }
      }
  }
   */
  //recursion은 함수형의 GOTO
  //대신에 이걸쓰고 FP 힙스터로 거듭나라고
//  List(a,b,c).foldRight(acc)(func)
//  func(a, func(b, func(c, acc)))

//
//  List(a,b,c).foldLeft(acc)(func)
//  func(func(func(acc, a), b), c)

  /* 이렇게
  {
    trait Future[T] {
        def retry(nTimes: Int)(block: => Future[T]): Future[T] = {
            val attempts = (1 to nTimes).map(_ => ()=>block)
            val failed = Future.failed(new Exception("oops"))
            val result = attempts.foldLeft(failed)(
                (fut, block) => fut.recoverWith { block() })
            result
          }
      }
  }

   */
  //하지만 항상 이게 더 나은 선택은 아닐수도 있다고. 위와 같이..그닥 안예쁨
  //recursion이 훨신 나음

  //====================================//
//  퓨처를 서용하기 전에 적성했던것처럼 그냥 일반적인 명령형 프로그램을 적성할 수 있게 해주는 것. 어싱크 어웨이트.
  //blocking처럼 생겼는데 아니다ㅋㅋㅋㅋㅋㅋㅋㅋ휴ㅋㅋ속지마
  // try-catch를 쓴다거나 등의 사용하면 안되는 경우 설명하고 예시

//def retry[T](nTimes: Int)(block: => Future[T]): Future[T] =
//  async { // block return Future
//    var i: Int = 0
//    var result: Try[T] = Failure(new Exception("Oops"))
//
//    while (i < nTimes && result.isFailure) {
//      result = await { Try(block) } // Future[Try[T]], result is Try[T]
  //    여기서 await은 기다리는게 아니라 바로 다음줄로 넘어가고, block실행이 끝나면 result에 넣어
  //    "continuation resume" 크하아아아
  //     callback으로 만들었다고 생각하면 onComplete를 이용했겠지
//      i += 1
//    }
//
//    result.get
//  }
//
//  // example
//  def filterI[T](future: Future[T], p: T => Boolean): Future[T] =
//      future onComplete { callback } => await이 없었으면 이렇게 했어야지
  //    async{
//      val x: T = await{ future }
//      if(!p(x)) { // predicate
//        throw new NoSuchElementException("No such element")
//      } else {
//        x
//      }
//    }
//
//  // example
//  def flatMap[T,S](future: Future[T], op: T => Future[S]): Future[S] =
//    async{
//      val x: T = await{ future }
//      await{ op(x) }: S
//    }

  //promise도있는데, 에릭 마이어 아저씨는 코드가 더 깔끔해서 에이싱크 어웨이트를 더 선호
  //scalaz의 Task
  //future(비동기 작업)의 결과를 밖에서 쓸 수 있능 것이 프로미스.
  //비동기 작업을 수행하는 쪽에서 주로 사용하지. 결과를 받는 쪽 보다는

  //======promise=====//

  // java의 CompletableFuture랑 비슷한거야 (javascript의 Promise랑 달라)
  //토비님 스프링캠프에서 AsyncRestTemplate에서 쓰려고 만든 것과 비슷
  //많은 현대 언어들에서 채택
  //def filter(pred: T=> Boolean): Future [T] = {
//    val p = Promise[T]()
//    this onComplete {
//        case Failure(e) => p.failure(e)
//        case Success(x) => if( !pred(x) ) p.failure( new NoSuch … )
//        else p.success(x) }
//    p.future }

//  def zip[S, R](that: Future[S], f: (T, S) => R): Future[R] = {
//      val p = Promise[R]()
//      this onComplete {
//          case Failure(e) => p.failure(e); case Success(x) => that onComplete {
//              case Failure(e) => p.failure(e); case Success(y) => p.success(f(x, y)) } }
//      p.future }

  // 얘가 훨 나음
//  def zip[S, R](that: Future[S], f: (T, S) => R): Future[R] = async {
//      f( await { this }, await { that } ) }

  //그럼 ㅇㅐ는 언제 유용할까? => 성능이 중요할 때 유용하게 쓸 수 있다.
  /* 누가 먼저 완료되는지 모를 때
  // single assignment variable 개념
  // 자바의 final같은거다
  def race[T](left: Future[T], right: Future[T]): Future[T] = {
    val p = Promise[T]()

    left  onComplete { p.tryComplete(_) } // 한번만 쓰고 끝나는거야. 그러니까 먼저 완료되는 애만 중하지.
    right onComplete { p.tryComplete(_) }

    p.future // 먼저 완료되는 애
  }
   */

// 마지막으로 언제 퓨처 플랫맵, 어싱크어웨이트, 프로미스 쓰면 좋은지 예시 하나씩 보여준 것.
  // 이건 퓨처가 훨씬 나은 것
  /*
  def sequenceI[T](fts: List[Future[T]]): Future[List[T]] = fts match {
    case Nil => Future(Nil)
    case h::t => h.flatMap(a => sequence(t))
        .flatMap(lst => Future(a::lst))
  }
   */
}

