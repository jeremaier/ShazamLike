package main

import scala.math._
import Array._
import Complex._
import Import._

object FFT {  
  def FillFile(wav2D : Array[Int], n : Int) : Array[Complex] = {
    var wav : Array[Complex] = ConvertSignalToComplex(wav2D, n)
    var sup2 = ArrayLength(wav.length)
    wav ++= Array.fill[Complex](sup2 - n)(Complex(0, 0))
    return FFTAnalysis(wav, sup2)
  }
  
  def ConvertSignalToComplex(wav2D : Array[Int], N : Int) : Array[Complex] = {
    var wavComplex : Array[Complex] = Array()
    var i : Int = 0
    while (i < N) {
      wavComplex :+= ConvertToComplex(wav2D(i))
      i += 1
    }
    return wavComplex
    
    /*
     * ////////////////Voulu faire une fonction recursive... -> StackOverFlow
    if(N == 0)
      return Array[Complex](ConvertToComplex(wav2D(0)))
    else return ConvertToComplex(wav2D.head) +: ConvertSignalToComplex(wav2D.tail, N - 1)
    */
  }
  
  def ArrayLength(length : Int) : Int = {
    var N : Int = length
    var n : Int = N
    if((n & (N - 1)) != 0)
      do {
        N = n
        n &= (n - 1)
      } while(n != 0)
    return N - n << 1
  }
  
  def FFTAnalysis(file : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return file
    if (N % 2 != 0) throw new RuntimeException("Problème de longueur pas puissance de 2!")
    
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