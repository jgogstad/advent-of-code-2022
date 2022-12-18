package jgogstad

import cats.data.Ior.{Both, Left, Right}
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import cats.syntax.all._

object Day10 extends IOApp {

  type Cycle = Long
  val input: Stream[IO, (String, Cycle)] = lines("day10.txt")
    .flatMap {
      case "noop"         => Stream.emits(List("noop"))
      case i @ s"addx $_" => Stream.emits(List.fill(2)(i))
    }
    .zipWithIndex

  val task1 = input
    .map { case (el, i) => el -> (i + 1) } // make it 1-indexed
    .partialScan((1L, 0L), false) {
      case ((x, n), (el, cycle)) if cycle == 20 || (cycle > 20 && (cycle - 20) % 40 == 0) =>
        (el, n) match {
          case ("noop", _) => {
            Both(x -> 0, (cycle, cycle * x))
          }
          case (s"addx ${int(a)}", 1) => {
            Both((x + a) -> 0, (cycle, cycle * x))
          }
          case (s"addx $_", _) => {
            Both(x -> (n + 1), (cycle, cycle * x))
          }
        }
      case (s, ("noop", _)) => {
        Left(s)
      }
      case ((x, 1), (s"addx ${long(a)}", _)) => {
        Left(x + a -> 0L)
      }
      case ((x, n), (s"addx $_", _)) => {
        Left(x -> (n + 1))
      }
    }
    .map(_._2)
    .foldMonoid
    .compile
    .lastOrError

  def crt(x: Long, cycle: Long): Char = List(x - 1, x, x + 1).filter(_ == (cycle % 40)).headOption.fold('.')(_ => '#')

  val task2 = input
    .mapAccumulate((1L, 0L)) { case ((x, n), (el, cycle)) =>
      (el, n) match {
        case ("noop", _) => {
          ((x -> n), (crt(x, cycle) -> cycle))
        }
        case (s"addx ${int(a)}", 1) => {
          (((x + a) -> 0L), (crt(x, cycle), cycle))
        }
        case (s"addx $_", _) => {
          (x -> (n + 1), (crt(x, cycle), cycle))
        }
      }
    }
    .map { case (_, (char, cycle)) =>
      if (cycle > 0 && (cycle + 1) % 40 == 0) s"$char\n" else char.toString
    }
    .foldMonoid
    .compile
    .lastOrError

  override def run(args: List[String]): IO[ExitCode] = 
    (task1, task2.map("\n" + _)).parTupled.flatTap(s => log.info(s.toString)).as(ExitCode.Success)
}
