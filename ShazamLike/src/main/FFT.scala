package main

import scala.math._
import Array._

object FFT {
  var file : Array[Int] = Array()
  var filesDirectory : Array[Array[Int]] = Array()
  var ham : Array[Double] = Array()
  
  def FileFFT(wav2D : Array[Array[Int]]) {
    //fichier a analyser wav ici en ce moment
    //file = FFT(wav2D)
  }
  
  def DirectoryFilesFFT(wav2DDirectory : Array[Array[Int]]) : Array[Array[Int]] = {
    //Les fichiers du dossier wav ici pour l'instant
    return wav2DDirectory
  }
  
  /*def FFT(file : Array[Array[Int]]) : Array[Float] = {
    val fe : Int = 44100
    var N : Int = file.length
 
    if (N == 1) return new Complex[] {x[0]}
    if (N % 2 != 0) {throw new RuntimeException("N is not a power of 2")}
    
    var even = new Complex(N / 2)
    for (k <- 0 to N / 2 - 1)
        even(k) = x(2 * k)
    var q = FFT(even)
    
    var odd = even
    for (k <- 0 to N / 2 - 1)
        odd[k] = x(2 * k + 1)
    var r = FFT(odd)
    
    var fft = new Complex(N)
    for (k <- 0 to N / 2 - 1) {
        var kth : Double = -2 * k * Pi / N
        var wk = new Complex(Math.cos(kth), Math.sin(kth))
        fft(k) = q[k].plus(wk.times(r[k]))
        fft(k + N / 2) = q[k].minus(wk.times(r[k]))
    }
    return fft
  }
  
  def plus(Complex b) : Complex = {
    Complex a = this; // invoking object
    double real = a.re + b.re;
    double imag = a.im + b.im;
    return new Complex(real, imag);
  }
 
  def minus(Complex b) : Complex = {
      Complex a = this;
      double real = a.re - b.re;
      double imag = a.im - b.im;
      return new Complex(real, imag);
  }
 
  def times(Complex b) : Complex = {
      Complex a = this;
      double real = a.re * b.re - a.im * b.im;
      double imag = a.re * b.im + a.im * b.re;
      return new Complex(real, imag);
  }*/
}