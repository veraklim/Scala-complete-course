package lectures.operators


import lectures.functions.{Computation, CurriedComputation, FunctionalComputation, Data}

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого
  *   * в классах CurriedComputation и FunctionalComputation уберите extends App, оставьте extends Data
  *   * раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  *   * оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {

  val computationStartTimestamp = System.currentTimeMillis()

//  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  for (i <- 1 to 100) {
    //print(i)
    Computation.computation(filterData, dataArray)
  }
//
  println("Elapsed time in computation(): " + (System.currentTimeMillis() - computationStartTimestamp)) //11226
//
//
//
  val partiallyAppliedStartTimestamp = System.currentTimeMillis()
//
//  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  for (i <- 1 to 100) {
    //print(i)
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }
//
  val partiallyAppliedDuration = System.currentTimeMillis() - partiallyAppliedStartTimestamp
  println("Elapsed time in partiallyAppliedCurriedFunction(): " + partiallyAppliedDuration) //11226
//
//
//
  val filterAppliedStartTimestamp = System.currentTimeMillis()
//
//  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  for (i <- 1 to 100) {
    //print(i)
    FunctionalComputation.filterApplied(dataArray)
  }
//
  val filterAppliedDuration = System.currentTimeMillis() - filterAppliedStartTimestamp
  println("Elapsed time in filterApplied():" + filterAppliedDuration) //115
//
//  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
//  // И ФУНКЦИОНАЛЬНОЙ
//
  val diff = partiallyAppliedDuration - filterAppliedDuration
//
  println(s"Difference is about $diff milliseconds") //11111
 
 //Такая разница потому что Thread.sleep(100) в functionalComputation запускалось 1 раз при инициализации 
 //val filterApplied = functionalComputation(filterData), и 100 запускался уже результат functionalComputation (то есть функция).
 //А partiallyAppliedCurriedFunction вычислялось каждый раз заново.
}

