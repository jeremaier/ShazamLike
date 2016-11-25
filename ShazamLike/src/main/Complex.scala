package main

import scala.math._

class Complex(val real : Double, val img : Double) {
  def unary_- = new Complex(this.real * -1, this.img * -1)
  
  def unary_~ = new Complex(this.real, this.img * -1)
  
  def +(that : Complex) = new Complex(this.real + that.real, this.img + that.img)
  
  def -(that : Complex) = this + -that
  
  def *(that : Complex) = {
    val realPart = this.real * that.real + -(this.img * that.img)
    val imgPart = this.real * that.img + this.img * that.real
    new Complex(realPart, imgPart)
  }
  
  def /(that : Complex) = {
    val numer = this * ~that
    val denom = that * ~that
    val divisor = denom.real
    new Complex(numer.real / divisor, numer.img / divisor)
  }
  
  implicit def ConvertToComplex(real : Double) = new Complex(real, 0.0)
  
  implicit class DoubleToComplex(num : Double) {
    def i = new Complex(0, num)
  }
  
  def Module(that : Complex) : Double = return hypot(that.real, that.img)
}

object Complex {
  def apply(real : Double, img : Double) = new Complex(real, img)
}