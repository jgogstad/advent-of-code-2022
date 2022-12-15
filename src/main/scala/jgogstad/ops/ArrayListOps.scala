package jgogstad.ops

import scala.collection.immutable.::

class ArrayListOps[A](el: Array[List[A]]) {

  private val rowCount = el.map(_.size).max
  private val colCount = el.size

  def apply(row: Int, cols: ::.type): Array[List[A]] = {
    apply(row to row, 0 until colCount)
  }

  def apply(rows: ::.type, col: Int): Array[List[A]] = {
    apply(0 until rowCount, col to col)
  }

  def apply(rows: Range, cols: Range): Array[List[A]] = {
    val targetRows = rows.map(r => if (r < 0) rowCount + r else r).toSet
    val targetCols = cols.map(c => if (c < 0) colCount + c else c).toSet

    val filteredCols = el.zipWithIndex.collect { case (el, i) if targetCols.contains(i) => el }
    filteredCols.map { a =>
      a.zipWithIndex.collect {
        case (el, i) if targetRows.contains(i) => el
        case (el, i) if i == a.length - 1 => el
      }
    }
  }
}
