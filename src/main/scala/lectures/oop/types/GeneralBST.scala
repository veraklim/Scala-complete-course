package lectures.oop.types

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}

case class GeneralBSTImpl[T](
    value: T,
    left: Option[GeneralBSTImpl[T]] = None,
    right: Option[GeneralBSTImpl[T]] = None)(implicit comparator: Comparator[T])
    extends GeneralBST[T] {

  override def find(value: T): Option[GeneralBST[T]] = findImpl(value)

  def findImpl(valueF: T): Option[GeneralBSTImpl[T]] =
    (valueF, left, right) match {
      case (v, l, r) if v == value =>
        Option(GeneralBSTImpl(value, left, right))
      case (v, None, _) if comparator.compare(v, value) < 0 => None
      case (v, Some(l), _) if comparator.compare(v, value) < 0 =>
        l.findImpl(valueF)
      case (v, _, None) if comparator.compare(v, value) > 0 => None
      case (v, _, Some(r)) if comparator.compare(v, value) > 0 =>
        r.findImpl(valueF)
    }

  override def add(newValue: T): GeneralBST[T] = addImpl(newValue)

  def addImpl(newValue: T): GeneralBSTImpl[T] = (value, left, right) match {
    case (cur, _, _) if cur == newValue => this
    case (cur, None, _) if comparator.compare(newValue, cur) < 0 =>
      GeneralBSTImpl(cur, Option(GeneralBSTImpl(newValue, None, None)), right)
    case (cur, Some(left), _) if comparator.compare(newValue, cur) < 0 =>
      GeneralBSTImpl(value, Option(left.addImpl(newValue)), right)
    case (cur, _, None) if comparator.compare(newValue, cur) > 0 =>
      GeneralBSTImpl(cur, left, Option(GeneralBSTImpl(newValue, None, None)))
    case (cur, _, Some(right)) if comparator.compare(newValue, cur) > 0 =>
      GeneralBSTImpl(value, left, Option(right.addImpl(newValue)))
  }
}

abstract class Comparator[T] {
  def compare(first: T, second: T): Int
}

object GeneralTree extends App {

  implicit val floatComparator = new Comparator[Float] {
    def compare(first: Float, second: Float): Int = first.compareTo(second)
  }
  implicit val stringComparator = new Comparator[String] {
    def compare(first: String, second: String): Int = first.compareTo(second)
  }

  implicit val watchesComparator = new Comparator[Watches] {
    def compare(first: Watches, second: Watches): Int = first.cost.compareTo(second.cost)
  }

  val sc = new java.util.Scanner(System.in)

  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toFloat
  val markerItem2 = (Math.random() * maxValue).toFloat
  val markerItem3 = (Math.random() * maxValue).toFloat

  val rootFL: GeneralBST[Float] = GeneralBSTImpl[Float](maxValue.toFloat / 2)
  val treeFL: GeneralBST[Float] = (1 until nodesCount).foldLeft(rootFL)((tree, _) => tree.add((Math.random() * maxValue).toFloat))// generator goes here

  println(treeFL)

  val markerItemSt = (Math.random() * maxValue).toString
  val markerItem2St = (Math.random() * maxValue).toString
  val markerItem3St = (Math.random() * maxValue).toString

  val rootST: GeneralBST[String] = GeneralBSTImpl[String]((maxValue.toInt / 2).toString)
  val treeST: GeneralBST[String] = (1 until nodesCount).foldLeft(rootST)((tree, _) => tree.add((Math.random() * maxValue).toString))// generator goes here

  println(treeST)

  val markerItemW = Watches("Adidas",(Math.random() * maxValue).toFloat)
  val markerItem2W = Watches("Abibas",(Math.random() * maxValue).toFloat)
  val markerItem3W = Watches("Ribok",(Math.random() * maxValue).toFloat)

  val rootW: GeneralBST[Watches] = GeneralBSTImpl[Watches](Watches("Convs",(maxValue.toInt / 2).toFloat))
  val treeW: GeneralBST[Watches] = (1 until nodesCount).foldLeft(rootW)((tree, _) => tree.add(Watches("Ribok",(Math.random() * maxValue).toFloat)))// generator goes here

  println(treeW)

}
