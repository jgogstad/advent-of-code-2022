package jgogstad

import cats.syntax.all._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pipe
import fs2.io.file.{Files, Path}
import utils._
import spire.algebra._
import spire.implicits._

object Day3 extends IOApp {
  val readInput = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day3.txt").getPath))
    .through(text.utf8.lines)
    .filter(_.nonEmpty)

  val valueOf: Char => Int = {
    case c if c.isUpper => c.toInt - 65 + 27
    case c              => c.toInt - 96
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val task1: Pipe[IO, String, Int] = _.map(s => s.splitAt(s.length / 2))
      .map { case (l, r) => l.toSet.intersect(r.toSet).toList }
      .map { case h :: Nil => valueOf(h) }
      .foldMonoid(AdditiveMonoid[Int].additive)
      .evalTap(i => log.info(show"task 1: $i"))

    val task2: Pipe[IO, String, Int] = _.groupAdjacentByLimit(3)(_ => true)
      .map { case (_, c) => c.toList }
      .map { case a :: b :: c :: Nil => a.toSet.intersect(b.toSet).intersect(c.toSet).toList }
      .map { case h :: Nil => valueOf(h) }
      .foldMonoid(AdditiveMonoid[Int].additive)
      .evalTap(i => log.info(show"task 2: $i"))

    readInput
      .broadcastThrough(task1, task2)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
