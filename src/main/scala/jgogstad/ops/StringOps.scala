package jgogstad.ops

class StringOps(s: String) {
  def dropRightWhile(f: Char => Boolean): String        = s.reverse.dropWhile(f).reverse
  def dropRightWhileThrough(f: Char => Boolean): String = s.reverse.dropWhile(f).drop(1).reverse
  def dropWhileThrough(f: Char => Boolean): String      = s.dropWhile(f).drop(1)
}
