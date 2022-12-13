package jgogstad

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import fs2.io.file.{Files, Path}
import cats.syntax.all._

object Day1 extends IOApp {
  val sums: Stream[IO, Int] = lines("day1/input.txt")
    .groupAdjacentBy(_.isEmpty)
    .evalMapFilter {
      case (false, c) => IO(c.toList.traverse(s => Option(s.toInt)).map(_.sum))
      case _          => IO(none[Int])
    }

  val task1 = sums.fold(0)(Math.max _).compile.lastOrError

  val task2: IO[Int] = sums
    .fold(List.empty[Int]) {
      case (one :: two :: three :: Nil, el) => List(one, two, three, el).sorted.drop(1)
      case (l, el)                          => el :: l
    }
    .map(_.sum)
    .compile
    .lastOrError

  override def run(args: List[String]): IO[ExitCode] = {
    val t1 = task1.map(i => show"t1: $i").flatTap(log.info(_))
    val t2 = task2.map(i => show"t2: $i").flatTap(log.info(_))
    (t1, t2).parTupled.as(ExitCode.Success)
  }
}
