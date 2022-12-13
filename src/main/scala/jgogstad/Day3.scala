package jgogstad

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.{Pipe, Stream}
import spire.implicits._

// set intersection
object Day3 extends IOApp {
  val readInput: Stream[IO, String] = lines("day3.txt").filter(_.nonEmpty)

  val valueOf: Char => Int = {
    case c if c.isUpper => c.toInt - 65 + 27
    case c              => c.toInt - 96
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val task1: Pipe[IO, String, Int] = _.map(s => s.splitAt(s.length / 2))
      .map { case (l, r) => l.toSet.intersect(r.toSet).toList }
      .map { case h :: Nil => valueOf(h) }
      .foldMonoid
      .evalTap(i => log.info(show"task 1: $i"))

    val task2: Pipe[IO, String, Int] = _.groupAdjacentByLimit(3)(_ => true)
      .map { case (_, c) => c.toList }
      .map { case a :: b :: c :: Nil => a.toSet.intersect(b.toSet).intersect(c.toSet).toList }
      .map { case h :: Nil => valueOf(h) }
      .foldMonoid
      .evalTap(i => log.info(show"task 2: $i"))

    readInput
      .broadcastThrough(task1, task2)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
