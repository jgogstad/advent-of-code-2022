package jgogstad.ops

import breeze.linalg.DenseMatrix
import breeze.math.Field
import breeze.storage.Zero
import spire.ClassTag
import cats.syntax.all._

class DenseMatrixOps[A: ClassTag: Field](matrix: DenseMatrix[A]) {

  def convolveMap[S, B: Zero](mask: DenseMatrix[B], default: Option[A] = None)(
    f: ((Int, Int), A, List[((Int, Int), A)]) => A
  ): DenseMatrix[A] =
    convolveAcc(mask, default, ())((_, c, a, m) => () -> f(c, a, m))._2

  def convolveAcc[S, B: Zero](mask: DenseMatrix[B], default: Option[A], z: S)(
    f: (S, (Int, Int), A, List[((Int, Int), A)]) => (S, A)
  ): (S, DenseMatrix[A]) = {
    val matrixCopy = matrix.copy
    val zero       = implicitly[Zero[B]].zero

    val maskRowRadius = mask.rows / 2
    val maskColRadius = mask.cols / 2

    val s = matrix.activeKeysIterator.foldLeft(z) { case (acc, ij @ (i, j)) =>
      val clampRows = Math.max(0, i - maskRowRadius)
      val clampCols = Math.max(0, j - maskColRadius)

      val window: DenseMatrix[A] = matrix(
        clampRows to Math.min(matrix.rows - 1, i + maskRowRadius),
        clampCols to Math.min(matrix.cols - 1, j + maskColRadius)
      )

      val maskUp    = Math.abs(Math.min(0, i - maskRowRadius))
      val maskDown  = Math.abs(Math.max(0, i + maskRowRadius - (matrix.rows - 1)))
      val maskLeft  = Math.abs(Math.min(0, j - maskColRadius))
      val maskRight = Math.abs(Math.max(0, j + maskColRadius - (matrix.cols - 1)))

      // aligned mask transformation at (0,0):
      // 0 0 1 0 0
      // 0 0 1 0 0      2  1  1
      // 1 1 2 1 1  =>  1  0  0
      // 0 0 1 0 0      1  0  0
      // 0 0 1 0 0
      val alignedMask     = mask(maskUp until (mask.rows - maskDown), maskLeft until (mask.cols - maskRight))
      val windowMaskPairs = window.activeIterator.zip(alignedMask.activeIterator).toList

      val overlappingCoordinates = windowMaskPairs.mapFilter { case (c, (_, include)) =>
        (include != zero).guard[Option].as(c).map { case ((p, q), a) =>
          ((p - maskRowRadius + maskUp + i), (q - maskColRadius + maskLeft + j)) -> a
        }
      }

      val outUp    = mask(0 until maskUp, ::)
      val outLeft  = mask(::, 0 until maskLeft)
      val outDown  = mask((mask.rows - maskDown) until mask.rows, ::)
      val outRight = mask(::, (mask.cols - maskRight) until mask.cols)

      val overflowCoordinates = default
        .fold(List.empty[((Int, Int), A)]) { d =>
          val upLeft = (outUp.activeIterator ++ outLeft.activeIterator).collect { case ((r, c), v) if v != zero =>
            (r - maskRowRadius + i, c - maskColRadius + j) -> d
          }.toList
          val downRight = (outDown.activeIterator ++ outRight.activeIterator).collect { case ((r, c), v) if v != zero =>
            (r - maskRowRadius + maskDown + i, c - maskColRadius + maskRight + j) -> d
          }.toList
          (upLeft ++ downRight).toList
        }
        .toList
        .distinct

      val (s, a) = f(acc, ij, matrix(ij), overlappingCoordinates ++ overflowCoordinates)
      matrixCopy.update(ij, a)
      s
    }
    s -> matrixCopy
  }

  /**
     * Pad the matrix with el
     *
     * Pads left, right, up, down
     */
  def pad(n: Int, el: A): DenseMatrix[A] = {
    val rows = DenseMatrix.create(n, matrix.cols, Array.fill(matrix.cols * n)(el), 0, matrix.cols, true)
    val m1   = DenseMatrix.vertcat(rows, matrix, rows)
    val cols = DenseMatrix.create(m1.rows, n, Array.fill(m1.rows * n)(el))
    DenseMatrix.horzcat(cols, m1, cols)
  }
}
