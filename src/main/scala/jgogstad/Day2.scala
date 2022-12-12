package jgogstad

import algebra.ring.AdditiveMonoid
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Pipe
import fs2.io.file.{Files, Path}
import jgogstad.utils._
import spire.implicits._

object Day2 extends IOApp {

  // given input, what are the possible outcomes
  val possibleOutcomes: Char => List[Int] = {
    case 'A' => List(1 + 3, 2 + 6, 3)
    case 'B' => List(1, 2 + 3, 3 + 6)
    case 'C' => List(1 + 6, 2, 3 + 3)
  }

  def normalRules(outcomes: List[Int]): Char => Int = {
    case 'X' => outcomes(0)
    case 'Y' => outcomes(1)
    case 'Z' => outcomes(2)
  }

  val choose: (Char, Char) => Int = {
    case (c, 'X') => possibleOutcomes(c).min
    case (c, 'Y') => possibleOutcomes(c).sorted.apply(1)
    case (c, 'Z') => possibleOutcomes(c).max
  }

  val readInput = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day2/input.txt").getPath))
    .through(text.utf8.lines)
    .filter(_.nonEmpty)
    .map(_.split(" "))
    .map { case Array(char(abc), char(xyz)) => abc -> xyz }

  override def run(args: List[String]): IO[ExitCode] = {
    val task1: Pipe[IO, (Char, Char), Int] =
      _.map(Function.uncurried(normalRules _ compose possibleOutcomes).tupled)
        .foldMonoid(AdditiveMonoid[Int].additive)
        .evalTap(i => log.info(show"task 1: $i"))

    val task2: Pipe[IO, (Char, Char), Int] =
      _.map(choose.tupled)
        .foldMonoid(AdditiveMonoid[Int].additive)
        .evalTap(i => log.info(show"task 2: $i"))

    readInput.broadcastThrough(task1, task2).compile.drain.as(ExitCode.Success)
  }

}
