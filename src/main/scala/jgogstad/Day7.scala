package jgogstad

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import cats.syntax.all._

object Day7 extends App {

  val readInput = linesUnsafe("day7.txt")

  implicit class StringPathOps(s: String) {
    def /(p: String): String = s"${s.stripSuffix("/")}/${p.stripPrefix("/")}"
  }

  // state machine: (cwd, path -> (IsDir, Size))
  val fsm: (String, Map[String, (Boolean, Long)]) = readInput
    .foldLeft("", Map.empty[String, (Boolean, Long)]) {
      case ((path, dirs), "$ cd ..") => path.dropRightWhileThrough(_ != '/') -> dirs
      case ((path, dirs), s"$$ cd $dir") =>
        val sub = if (dir.startsWith("/")) dir else (path / dir)
        sub -> dirs.updatedWith(sub)(_.fold((true -> 0L).some)(_.some))
      case (state, "$ ls")      => state
      case (state, s"dir $_") => state
      case ((path, dirs), s"${long(size)} $file") =>
        // .inits gives all subpaths
        val tree = (path / file).split("/").inits.map(_.foldLeft("")(_ / _)).toList.filter(_.nonEmpty)
        path -> tree
          .foldLeft(dirs)(_.updatedWith(_)(_.fold((false, size).some)(s => (s._1, s._2 + size).some)))
          .updated(path / file, (false, size))
    }

  val task1 = fsm.collect {
    case (k, (true, size)) if size <= 100000 => size
  }.sum

  val task2 = {
    val size = 70000000L
    val needed = 30000000L
    val free = size - fsm("/")._2
    val toFree = needed - free

    val result = fsm.collect {
      case (k, (true, size)) if size >= toFree => k -> size
    }.minBy(_._2)
    println(result)
  }

  println(task1)
}
