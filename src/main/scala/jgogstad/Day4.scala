package jgogstad

import algebra.ring.AdditiveMonoid
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.{Pipe, Stream}
import spire.implicits._

// range overlaps
object Day4 extends IOApp {

  val readInput: Stream[IO, (Range.Inclusive, Range.Inclusive)] = lines("day4.txt").filter(_.nonEmpty).map {
      case s"${a}-${b},${c}-${d}" => (a.toInt to b.toInt) -> (c.toInt to d.toInt)
    }

  override def run(args: List[String]): IO[ExitCode] = {
    val task1: Pipe[IO, (Range, Range), Int] =
      _.filter { case (x, y) => Set(x.length, y.length).contains(x.intersect(y).length) }
        .foldMap(_ => 1)(AdditiveMonoid[Int].additive)
        .evalTap(a => log.info(show"task 1: $a"))

    val task2: Pipe[IO, (Range, Range), Int] =
      _.filter { case (x, y) => x.intersect(y).nonEmpty }
        .foldMap(_ => 1)(AdditiveMonoid[Int].additive)
        .evalTap(a => log.info(show"task 2: $a"))

    readInput.broadcastThrough(task1, task2).compile.drain.as(ExitCode.Success)
  }
}
