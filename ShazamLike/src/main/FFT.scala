package main

import scala.math._
import Array._
import Complex._

object FFT {
  var filesDirectory : Array[Array[Complex]] = Array()
  
  def FileFFT(wav2D : Array[Int], n : Int, directory : Boolean) {
    if(directory == true)
      filesDirectory :+ FFTAnalysis(ConvertSignalToComplex(wav2D, n), n)
    else {
      wav2D :+ Array.fill[Complex](ArrayLength(wav2D.length))(Complex(0, 0))
      var fft = FFTAnalysis(ConvertSignalToComplex(wav2D, n), n)
      print("FFT fichier : " + fft)      ///////////////////////
    }
  }
  
  def ConvertSignalToComplex(wav2D : Array[Int], n : Int) : Array[Complex] = {
    var wav2DComplex : Array[Complex] = Array()
    for(i <- 0 to n)
      wav2DComplex :+ ConvertToComplex(wav2D(i))
    return wav2DComplex
  }
  
  def DirectoryFilesFFT(wav2DDirectory : Array[Int], n : Int) {
    wav2DDirectory :+ Array.fill[Complex](ArrayLength(wav2DDirectory.length))(Complex(0, 0))
    FileFFT(wav2DDirectory, n, true)
    print(filesDirectory)                /////////////////////////
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
}