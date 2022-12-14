package jgogstad

import breeze.linalg.DenseMatrix
import cats.syntax.all._
import jgogstad.util.Masks

object Day8 extends App {

  val input: DenseMatrix[Int] = {
    val lines = linesUnsafe("day8.txt").map(_.toList.map(_.toString.toInt))
    DenseMatrix(lines: _*)
  }

  val mask: DenseMatrix[Int] = Masks.cross[Int](input.rows * 2 + 1 , input.cols * 2 + 1, Some(0))

  def task1 = input.convolveAcc(mask, Some(-1), List.empty[(Int, (Int, Int))]) { case (acc, ij@(i, j), a, neighbours) =>
    val (left, right) = neighbours.collect {
      case ((m, n), v) if n < j => Left(v)
      case ((m, n), v) if n > j => Right(v)
    }.separate
    val (up, down) = neighbours.collect {
      case ((m, n), v) if m < i => Left(v)
      case ((m, n), v) if m > i => Right(v)
    }.separate

    if (left.forall(_ < a) || right.forall(_ < a) || up.forall(_ < a) || down.forall(_ < a))
      ((a, ij) :: acc) -> 0
    else acc -> 0
  }

  val task2 = input.convolveMap(mask, None) { case (ij@(i, j), a, neighbours) =>
    val (left, right) = neighbours.collect {
      case ((m, n), v) if n < j => Left(v)
      case ((m, n), v) if n > j => Right(v)
    }.separate
    val (up, down) = neighbours.collect {
      case ((m, n), v) if m < i => Left(v)
      case ((m, n), v) if m > i => Right(v)
    }.separate

    List(left.reverse, right, up.reverse, down).map(_.takeWhileThrough(_ < a).length).reduce(_ * _)
  }

  println(task1._1.length)
  println(task2.activeIterator.maxBy(_._2)._2)
}
