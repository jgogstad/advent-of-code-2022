package jgogstad

import breeze.linalg.DenseMatrix.FrobeniusInnerProductDenseMatrixSpace
import spire.math.Complex
import spire.algebra._
import spire.implicits._

import scala.collection.immutable.MultiDict

object Day9 extends App {

  val input: List[Complex[Int]] = linesUnsafe("day9.txt").map {
    case s"R ${int(i)}" => Complex(i, 0)
    case s"U ${int(i)}" => Complex(0, i)
    case s"L ${int(i)}" => Complex(-i, 0)
    case s"D ${int(i)}" => Complex(0, -i)
  }

  def execute(state: Array[List[Complex[Int]]], instruction: Complex[Int]): Array[List[Complex[Int]]] = {
    def go(state: Array[List[Complex[Int]]], el: Int): Array[List[Complex[Int]]] = {
      if (el == 0) go(state.updated(0, (state(0)(0) + instruction.basis) :: state(0)), el + 1)
      else if (el >= state.length) state
      else {
        val previous = state(el - 1)(0)
        val window   = previous.window(1)

        val current = state(el)(0)
        if (window.contains(current)) go(state, el + 1)
        else {
          val align     = (previous - current).basis
          val next      = current + align
          val nextState = state.updated(el, next :: state(el))

          go(nextState, el + 1)
        }
      }
    }

    val steps = if (instruction.real != 0) instruction.real else instruction.imag
    (0 until steps by (steps / steps.abs)).map(Complex(_, 0)).foldLeft(state) { case (s, _) => go(s, 0) }
  }

  val task1 = input.foldLeft(Array(List(Complex(0, 0)), List(Complex(0, 0))))(execute)
  val task2 = input.foldLeft(Array((0 to 9).map(_ => List(Complex(0, 0))).toList: _*))(execute)

  println(task1(1).toSet.size)
  println(task2(9).toSet.size)

}
