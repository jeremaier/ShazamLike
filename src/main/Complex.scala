package main

import scala.math._

class Complex(val real : Double, val img : Double) {
  //Oppose d'un complexe
  def unary_- : Complex = new Complex(this.real * -1, this.img * -1)
  
  //Addition de 2 nombres complexes
  def +(that : Complex) : Complex = new Complex(this.real + that.real, this.img + that.img)
  
  //Soustraction de 2 nombres complexes
  def -(that : Complex) : Complex = this + -that
  
  //Multiplication de 2 nombres complexes
  def *(that : Complex) : Complex = {
    val realPart = this.real * that.real + -(this.img * that.img)
    val imgPart = this.real * that.img + this.img * that.real
    new Complex(realPart, imgPart)
  }
  
  //Module d'un nombre complexe
  def mod() : Double = return hypot(this.real, this.img)
}

object Complex {
  def apply(real : Double, img : Double) : Complex = new Complex(real, img)
  
  //Converti en complexe un nombre reel
  def ConvertToComplex(real : Double) : Complex = new Complex(real, 0.0)
}