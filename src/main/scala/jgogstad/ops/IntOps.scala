package jgogstad.ops

class IntOps(i: Int) {
  def clamp(min: Int, max: Int): Int = Math.min(Math.max(i, min), max)
}
