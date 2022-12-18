import algebra.ring.Semiring
import breeze.linalg.DenseMatrix
import breeze.math.Field
import cats.Show
import cats.effect.IO
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import fs2.{Pipe, Stream}
import jgogstad.ops.{ArrayListOps, ComplexIntOps, DenseMatrixOps, Fs2StreamOps, IntOps, ListOps, StringOps}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import spire.ClassTag
import spire.math.{Complex, SafeLong}
import spire.syntax.all._

import scala.io.Source

package object jgogstad {

  def lines(path: String): Stream[IO, String] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource(path).getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)

  def linesUnsafe(path: String): List[String] =
    Source.fromURL(getClass.getClassLoader.getResource(path)).getLines().toList

  implicit def showTuple[A: Show, B: Show](t2: Tuple2[A, B]): Show[Tuple2[A, B]] = { case (a, b) => show"($a -> $b)" }
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa =>
    ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString
  implicit def showSafeLong: Show[SafeLong]              = Show.fromToString[SafeLong]

  val log = Slf4jLogger.getLogger[IO]

  implicit def denseMatrixOps[A: ClassTag: Field](m: DenseMatrix[A]): DenseMatrixOps[A] = new DenseMatrixOps[A](m)

  implicit def stringOps(s: String): StringOps = new StringOps(s)
  implicit def intOps(i: Int): IntOps          = new IntOps(i)
  implicit def listOps[A](l: List[A]): ListOps[A]          = new ListOps(l)
  implicit def arrayListOps[A](aa: Array[List[A]]): ArrayListOps[A] = new ArrayListOps[A](aa)
  implicit def complexOps(c: Complex[Int]): ComplexIntOps = new ComplexIntOps(c)
  implicit def fs2StreamOps[F[_], O](stream: fs2.Stream[F, O]): Fs2StreamOps[F, O] = new Fs2StreamOps(stream)

  object char {
    def unapply(s: String): Option[Char] = {
      s.toCharArray.toList match {
        case h :: Nil => Some(h)
        case Nil      => None
      }
    }
  }
  object chars    { def unapplySeq(s: String): Option[Seq[Char]] = if (s.nonEmpty) Some(s.toCharArray.toSeq) else None }
  object long     { def unapply(s: String): Option[Long] = s.toLongOption                                              }
  object int      { def unapply(s: String): Option[Int] = s.toIntOption                                                }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong)                        }
}
