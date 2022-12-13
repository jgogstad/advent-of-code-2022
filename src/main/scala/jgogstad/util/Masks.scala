package jgogstad.util

import breeze.linalg.DenseMatrix
import breeze.math.Semiring
import breeze.storage.Zero
import spire.ClassTag

object Masks {

  /**
   * Example 3x3
   *
   * 0 1 0
   * 1 0 1
   * 0 1 0
   */
  def emptyCross[A: ClassTag: Zero: Semiring](rows: Int, cols: Int): DenseMatrix[A] = {
    val m      = cross[A](rows, cols)
    val middle = (rows / 2, cols / 2)
    m.update(middle, implicitly[Zero[A]].zero)
    m
  }

  /**
   * Example 3x3
   *
   * 0 1 0
   * 1 1 1
   * 0 1 0
   */
  def cross[A: ClassTag: Zero: Semiring](rows: Int, cols: Int): DenseMatrix[A] = {
    if (rows % 2 == 0 || cols % 2 == 0) throw new Exception("Need odd numbered rows and columns for cross mask")
    val rowMiddle = rows / 2
    val colMiddle = cols / 2

    val m   = DenseMatrix.zeros[A](rows, cols)
    val one = implicitly[Semiring[A]].one

    for (i <- 0 until cols) m.update(rowMiddle, i, one)
    for (i <- 0 until rows) m.update(i, colMiddle, one)
    m
  }
}