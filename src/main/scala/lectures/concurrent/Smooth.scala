package lectures.concurrent

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Smooth - это своебразный функциональный кэш, предназначенный для исключения повторных вызовов кода
  * до того, как получен результат первого вызова.
  * Он работает следующим образом:
  * * * * в объект Smooth в метод apply передается код, который может выполняться какое-то время, и возвращает какое-то значение
  * * * * apply создаст инстанс Smooth
  * * * * созданный инстанс при вызове apply возвращает Future
  * * * * * и запускает код, если код еще не запущен
  * * * * * и не запускает код, если код еще не завершился с момента предыдущего запуска
  *
  * Подсказка: можно использовать AtomicReference
  *
  */
class Smooth private (thunk: => Any) {
  private val atom: AtomicReference[Future[_]] =
    new AtomicReference[Future[_]](Future(thunk))

  def apply(): Future[_] = {
    if (atom.get().isCompleted) {
      atom.set(Future(thunk))
    }
    atom.get()
  }
}

object Smooth {
  def apply(thunk: => Any): Smooth = new Smooth(thunk)
}

object Main extends App {
  val work: Int => Unit = (num: Int) => println("Done " + num.toString)
  val sm1 = Smooth.apply(work(1))
  val sm2 = Smooth.apply(work(2))
  val sm3 = Smooth.apply(work(3))
  Await.result(sm1.apply(), 1.second)
  // Await.result(sm1.apply(), 1.second)
  Await.result(sm2.apply(), 30.seconds)
  // Await.result(sm2.apply(), 1.second)
  Await.result(sm3.apply(), 50.seconds)
  //  Await.result(sm3.apply(), 1.second)
}
