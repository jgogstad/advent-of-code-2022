package jgogstad.ops

import cats.data.Ior
import cats.data.Ior.{Both, Left, Right}
import fs2.{Pull, Stream}

class Fs2StreamOps[+F[_], +O](stream: fs2.Stream[F, O]) {

  /** Left fold that outputs some results
   *
   * The fold is controlled with an inclusive-or, [[cats.data.Ior]]. The folding function may output:
   * - Left(b1: B) continue fold with b1
   * - Right(b1: B) output b1 to stream, fold restarts with next stream element
   * - Both(b1, b2) output b2 to stream and continue fold with b1
   *
   * Stream.of(1,1,2,3,5,8).foldAccumulate(List.empty[Int]) { (acc, el) =>
   *   if (el % 2 == 0) Both(List(el), acc) else Left(el :: acc)
   * })
   *
   * // List(List(), List(0, 1, 1), List(2, 3, 5), List(8))
   *
   * @param emitFinal Whether the final value should be emitted or not
   */
  def partialScan[O2](z: O2, emitFinal: Boolean = true)(f: (O2, O) => Ior[O2, O2]): Stream[F, O2] = {
    def go(maybeCurrentAcc: Option[O2], s: Stream[F, O]): Pull[F, O2, Unit] =
      s.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          val currentAcc = maybeCurrentAcc.getOrElse(z)
          f(currentAcc, head) match {
            case Left(next)      => go(Some(next), tail)
            case Right(out)      => Pull.output1(out) >> go(None, tail)
            case Both(next, out) => Pull.output1(out) >> go(Some(next), tail)
          }

        case None =>
          maybeCurrentAcc match {
            case Some(acc) => {
              if (emitFinal) Pull.output1(acc) >> Pull.done else Pull.done
            }
            case None => Pull.done
          }
      }

    go(None, stream).stream
  }

}
