package main

import scala.math._
import Array._
import Complex._
import Import._

object FFT {  
  def FillFile(wav2D : Array[Array[Int]], n : Int) : Array[Complex] = {
    wav2D :+ fill[Complex](ArrayLength(wav2D(1).length))(Complex(0, 0))
    return FFTAnalysis(ConvertSignalToComplex(wav2D(1), n), n)
  }
  
  def ConvertSignalToComplex(wav2D : Array[Int], N : Int) : Array[Complex] = {
    var wav2DComplex : Array[Complex] = Array()
    for(i <- 0 to N)
      wav2DComplex :+ ConvertToComplex(wav2D(i))
    return wav2DComplex
  }
  
  def ArrayLength(length : Int) : Int = {
    var N : Int = length
    var n : Int = N
    if((n & (N - 1)) != 0) {
      do {
        N = n
        n &= (n - 1)
      } while(n != 0)
    }
    return N - n << 1
  }
  
  def FFTAnalysis(file : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return file
    if (N % 2 != 0) throw new RuntimeException("Problème de longueur pas puissance de 2!!! -> à corriger")
    
    var even : Array[Complex] = new Array(N / 2)
    for (k <- 0 to N / 2 by 2)
        even(k) = file(k)
    var fftEven : Array[Complex] = FFTAnalysis(even, N / 2)
    
    var odd : Array[Complex] = even
    for (k <- 0 to N / 2 by 2)
        odd(k) = file(k + 1)
    var fftOdd : Array[Complex] = FFTAnalysis(odd, N / 2)
    
    var fft : Array[Complex] = new Array(N)
    for (k <- 0 to N / 2) {
        var kth : Double = - 2 * k * Pi / N
        var wk = new Complex(Math.cos(kth), Math.sin(kth))
        fft(k) = fftEven(k) + (wk * fftOdd(k))
        fft(k + N / 2) = fftEven(k) + -(wk * fftOdd(k))
    }
    return fft
  }
  
  def ModuleFFT(fft : Array[Complex], N : Int) : Array[Double] = {
    var module : Array[Double] = new Array(N)
    for (i <- 0 to N)
      module(i) = fft(i).Module()
    return module
  }
}