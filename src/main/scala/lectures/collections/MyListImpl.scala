package lectures.collections

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList(data: List[Int]) {

    def flatMap(f: (Int => MyList)) =
      MyList(data.flatMap(inp => f(inp).data))

    def map(f: (Int) => Int) = this.flatMap(x => MyList(List(f(x))))

    def foldLeft(acc: Int)(f: ((Int, Int) => Int)): Int = this.data match {
      case head :: tail => tail.foldLeft(f(acc, head))(f)
      case head :: Nil => f(acc, head)
      case Nil => 0
    }

    def filter(f: Int => Boolean) = this.flatMap(x => {
      if (f(x)) MyList(List(x))
      else MyList(List[Int]())
    })
  }

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((a, b) => a + b) == 21)
  require(MyList(Nil).foldLeft(0)((a, b) => a + b) == 0)

}
