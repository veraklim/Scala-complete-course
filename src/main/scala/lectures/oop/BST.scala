package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(acc: Int)(f: (Int, Int) => (Int)): Int

  def toString(): String
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def addImpl(newValue: Int): BSTImpl = (value, left, right) match {
    case (cur, _, _) if cur == newValue => this
    case (cur, None, _) if newValue < cur => BSTImpl(cur, Option(BSTImpl(newValue, None, None)), right)
    case (cur, Some(left), _) if newValue < cur => BSTImpl(value, Option(left.addImpl(newValue)), right)
    case (cur, _, None) if newValue > cur =>  BSTImpl(cur, left, Option(BSTImpl(newValue, None, None)))
    case (cur, _, Some(right)) if newValue > cur => BSTImpl(value, left, Option(right.addImpl(newValue)))
  }

  override def add(newValue: Int): BST = this.addImpl(newValue)

  def findImpl(valueF: Int): Option[BSTImpl] = (valueF, left, right) match {
    case (v, l, r) if v == value => Option(BSTImpl(value, left, right))
    case (v, None, _) if v < value => None
    case (v, Some(l), _) if v < value => l.findImpl(valueF)
    case (v, _, None) if v > value => None
    case (v, _, Some(r)) if v > value => r.findImpl(valueF)
  }

  override def find(value: Int): Option[BST] = this.findImpl(value)

  override def fold(acc: Int)(f: (Int, Int) => (Int)): Int = f(acc, foldImpl(f))

  def foldImpl(f: (Int, Int) => (Int)): Int = (value, left, right) match {
    case (v, Some(l), Some(r)) => f(l.foldImpl(f), f(v, r.foldImpl(f)))
    case (v, Some(l), None) => f(v, l.foldImpl(f))
    case (v, None, Some(r)) => f(v, r.foldImpl(f))
    case (v, None, None) => v
  }

  def height: Int = 1 + Math.max(left.fold(0)(_.height), right.fold(0)(_.height))

  def build(graph: Array[Array[String]], i: Int, start: Int, end: Int): Unit = {
    graph(i)((start + end) / 2) = value.toString
    left.foreach(_.build(graph, i + 1, start, (start + end) / 2))
    right.foreach(_.build(graph, i + 1, (start + end + 1) / 2, end))
  }

  def str: String = {
    val graph: Array[Array[String]] = Array.fill(height)(Array.fill((1 << height) - 1)(" "*6))
    println(graph)
    this.build(graph, 0, 0, graph(0).length)
    graph.map(line => line.mkString).mkString("\n")
  }

  override def toString() = str

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 until nodesCount).foldLeft(root)((tree, _) => tree.add((Math.random() * maxValue).toInt))// generator goes here

  println(tree)
  val testTree = tree.add(markerItem)
  val t = testTree.add(markerItem2)
  val r = t.add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

}
