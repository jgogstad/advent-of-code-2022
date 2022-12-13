package jgogstad

import cats.effect.{ExitCode, IO, IOApp}

// matrix transposition
object Day5 extends IOApp {
  val parseInput: IO[(List[Array[Char]], List[String])] = lines("day5.txt")
    .groupAdjacentBy(_.nonEmpty)
    .map(_._2.toList.filter(_.nonEmpty))
    .filter(_.nonEmpty)
    .foldMap(a => List(a))
    .last
    .map { case Some(load :: instructions :: Nil) =>
      val max = load.map(_.length).max
      val stacks: List[Array[Char]] = load
        .map(_.padTo(max, ' '))
        .transpose
        .filter(s => s.last > '0' && s.last <= '9')
        .map(_.filter(c => !c.isWhitespace))
        .map(_.init.toArray) // last element is the stack id

      stacks -> instructions
    }
    .compile
    .lastOrError

  override def run(args: List[String]): IO[ExitCode] =
    parseInput.flatMap { case (stacks, instructions) =>
      def task1(n: Int, stack: Array[Char]): Array[Char] = stack.take(n).reverse
      def task2(n: Int, stack: Array[Char]): Array[Char] = stack.take(n)

      def solve(grabber: (Int, Array[Char]) => Array[Char]): String = instructions.foldLeft(stacks) { case (acc, s"move ${int(a)} from ${int(b)} to ${int(c)}") =>
        acc
          .updated(c - 1, grabber(a, acc(b - 1)) ++ acc(c - 1))
          .updated(b - 1, acc(b - 1).drop(a))
      }.map(_.head).mkString

      (log.info(solve(task1 _)) >> log.info(solve(task2 _))).as(ExitCode.Success)
    }

}
