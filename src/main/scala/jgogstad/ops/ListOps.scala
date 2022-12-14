package jgogstad.ops

class ListOps[A](l: List[A]){

  def takeWhileThrough(p: A => Boolean): List[A] = {
    val r = l.takeWhile(p)
    val next = l.dropWhile(p).take(1)
    r ::: next
  }

}
