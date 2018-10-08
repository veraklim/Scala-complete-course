package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  private def merge(data1: Seq[Int], data2: Seq[Int]): Seq[Int] = data1 match {
    case Nil => data2
    case data1 => data2 match {
      case Nil => data1
      case data2 => {
        if (data1.head <= data2.head) merge(data1.tail, data1.head +: data2)
        else merge(data1.tail, data2.head +: merge(Seq(data1.head), data2.tail))
      }
    }
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val len = data.length
    if (len <= 1) data
    else {
      merge(mergeSort(data.slice(0, len / 2)), mergeSort(data.slice(len / 2, len)))
    }
  }

 require(mergeSort(Seq(4, 8, 5, 1, 6, 0)) == List(0, 1, 4, 5, 6, 8))
}
