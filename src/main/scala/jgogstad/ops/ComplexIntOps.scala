package jgogstad.ops

import spire.math.Complex

class ComplexIntOps(c: Complex[Int]) {
  // inner product
  def <*>(rhs: Complex[Int]): Complex[Int] =
    Complex(c.real * rhs.real, c.imag * rhs.imag)

  lazy val absoluteValue: Complex[Int] = Complex(Math.abs(c.real), Math.abs(c.imag))

  lazy val flip: Complex[Int] = Complex(c.imag, c.real)

  lazy val basis: Complex[Int] = Complex(c.real / c.real.abs.max(1), c.imag / c.imag.abs.max(1))

  def window(radius: Int): List[Complex[Int]] = ((c.real - radius) to (c.real + radius)).flatMap(r => ((c.imag - radius) to (c.imag + radius)).map(Complex(r, _))).toList

}
