package main

import scala.math._
import Array._
import java.util.Arrays.copyOfRange
import Complex._
import Import._

object FFT {
  var hamming : Array[Double] = new Array(sampleLength)
  
  //Decoupe le son à analyser en des samples de 4096 amplitudes et effectue la FFT sur chacune d'elle après avoir appliqué hamming pour retourner la liste de toutes ces FFT
  def SplitingAndFFT(wav2D : Array[Double], N : Int, sampleL : Int) : Array[Double] = {
    val samples : Int = N / sampleL
    val lastSampleLength : Int = N % sampleL
    val last : Array[Complex] = FillFile(ConvertSignalToComplex(copyOfRange(wav2D, N - lastSampleLength, N), lastSampleLength), sampleLength)
    var FFT : Array[Double] = Array[Double]()
    
    for(i <- 0 to samples - 1) {
      var sample : Array[Complex] = new Array(sampleL)
      
      for(j <- 0 to sampleL - 1)
        sample(j) = ConvertToComplex(wav2D(sampleL * i + j) * hamming(j))
      
      FFT ++= ModuleFFT(sample, sampleL)
    }
    
    if(N % sampleLength != 0)
      FFT ++= ModuleFFT(last, sampleLength)
    
    return FFT
  }
    
  //Fenetre de Hamming
  def Hamming(sampleLength : Int) {
    val piSurN : Double = 2 * Pi / (sampleLength - 1)
    
    for(i <- 0 to sampleLength - 1)
      hamming(i) = 0.54 - 0.46 * cos(piSurN * i)
  }
  
  //Appelle la fonction FFT avec comme argument les parties du fichier de la bonne longueur (en puissance de 2)
  def FillFile(last : Array[Complex], sampleLength : Int) : Array[Complex] = {
    val wavLength = last.length
    var wav : Array[Complex] = new Array(sampleLength)
    wav = last ++ Array.fill[Complex](sampleLength - wavLength)(Complex(0, 0))
    return wav
  }
  
  //Converti chaque nombre du tableau des amplitudes du fichier en complexe
  def ConvertSignalToComplex(wav2D : Array[Double], N : Int) : Array[Complex] = {
    val wavComplex : Array[Complex] = new Array(N)
    
    for(i <- 0 to N - 1)
      wavComplex(i) = ConvertToComplex(wav2D(i))
    
    return wavComplex
  }
  
  //FFT d'un sample, ressort une liste de frequences
  def FFTAnalysis(sample : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return sample
    
    //Premiere liste de nombres d'indices pairs
    val even : Array[Complex] = new Array(N / 2)
    for (k <- 0 to (N - 1) / 2)
      even(k) = sample(2 * k)
    val fftEven : Array[Complex] = FFTAnalysis(even, N / 2)
    
    //Deuxieme liste de nombres d'indices impairs
    val odd : Array[Complex] = new Array(N / 2)
    for (k <- 0 to (N - 1) / 2)
      odd(k) = sample(2 * k + 1)
    val fftOdd : Array[Complex] = FFTAnalysis(odd, N / 2)
    
    //Calcul de la fft par Cooley-Tuckey
    var fft : Array[Complex] = new Array(N)
    for (k <- 0 to N / 2 - 1) {
      var kth : Double = - 2 * k * Pi / N
      var wk : Complex = new Complex(cos(kth), sin(kth))
      var wkOdd : Complex = wk * fftOdd(k)
      fft(k) = fftEven(k) + wkOdd
      fft(k + N / 2) = fftEven(k) + -wkOdd
    }
    return fft
  }
  
  //Module de chaque element de la fft
  def ModuleFFT(sample : Array[Complex], sampleLength : Int) : Array[Double] = {
    val module : Array[Double] = new Array(sampleLength)
    val fft : Array[Complex] = FFTAnalysis(sample, sampleLength)
    
    for(i <- 0 to sampleLength - 1)
      module(i) = fft(i).mod()
    
    return module
  }
}