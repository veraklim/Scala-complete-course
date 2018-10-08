package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо
  * исправить ошибки компиляции
  * напечатать:
  * * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * * "Продули", если победили приезжие
  * * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  * В целях упрощения можно поменять тип исходных данных
  */

object Competition extends App {

  val locals = Map("Artem" -> 6, "Sergey" -> 5D, "Anton" -> '2', "Vladimir" -> "2", "Alexander" -> 4L)
  val foreigners = Map[String, Int]("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

  def toInt[T](a: T): Int = {
    val reg1 = ("[0-9]+[.0]*").r
    val reg2 = ("[0-9]+").r
    a.toString() match {
      case reg1() => a.toString() match {
        case reg2() => a.toString().toInt //все "вида" Integer
        case _ => a.asInstanceOf[Double].toInt //Double
      }
      case _ => {
        throw new Exception("Wrong type")
      }
    }
  }

  val results = for (l <- locals;
                     f <- foreigners) yield {
    (l._1 + " vs " + f._1, toInt(l._2) - f._2)
  }

  //println(results)
  var finalResult = 0
  for (r <- results) {
    if (r._2 > 0) finalResult = finalResult + 1
    else finalResult = finalResult - 1
  }
  //println(finalResult)

  if (finalResult > 0) println("Наша взяла")
  else if (finalResult < 0) println("Продули")
  else println("Победила дружба")
}
