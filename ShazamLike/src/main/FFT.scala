package main

import scala.math._
import Array._
import Complex._

object FFT {
  var filesDirectory : Array[Float] = Array()
  
  def FileFFT(wav2D : Array[Int], n : Int, directory : Boolean) {
    //fichier a analyser ici
    if(directory == true)
      filesDirectory :+ FFT(ConvertSignalToComplex(wav2D, n), n)
    //else Compare(wav2D, n)
  }
  
  def ConvertSignalToComplex(wav2D : Array[Int], n : Int) : Array[Complex] = {
    var wav2DComplex : Array[Complex] = Array()
    for(i <- 0 to n)
      wav2DComplex :+ ConvertToComplex(wav2D(i))
    return wav2DComplex
  }
  
  def DirectoryFilesFFT(wav2DDirectory : Array[Int], n : Int) {
    var N : Int = wav2DDirectory.length
    var n : Int = N
    if((n & (N - 1)) != 0) {
      do {
        N = n
        n &= (n - 1)
      } while(n != 0)
    }
    
    wav2DDirectory :+ Array.fill[Complex](N - (n << 1))(Complex(0,0))
    FileFFT(wav2DDirectory, n, true)
  }
  
  def FFT(file : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return file
    if (N % 2 != 0) {throw new RuntimeException("N is not a power of 2")}
    
    var even : Array[Complex] = new Array(N / 2)
    for (k <- 0 to N / 2)
        even(k) = file(2 * k)
    var fftEven : Array[Complex] = FFT(even, N / 2)
    
    var odd : Array[Complex] = even
    for (k <- 0 to N / 2)
        odd(k) = file(2 * k + 1)
    var fftOdd : Array[Complex] = FFT(odd, N / 2)
    
    var fft : Array[Complex] = new Array(N)
    for (k <- 0 to N / 2) {
        var kth : Double = -2 * k * Pi / N
        var wk = new Complex(Math.cos(kth), Math.sin(kth))
        fft(k) = fftEven(k).+(wk.*(fftOdd(k)))
        fft(k + N / 2) = fftEven(k).+(-wk.*(fftOdd(k)))
    }
    return fft
  }
}