package main

import scala.math._
import Array._
import java.util.Arrays.copyOfRange
import Complex._
import Import._

object FFT {
  var hamming : Array[Double] = Array(sampleLength)
  
  //Decoupe le son à analyser en des samples de 4096 amplitudes et effectue la FFT sur chacune d'elle après avoir appliqué hamming pour retourner la liste de toutes ces FFT
  def SplitingAndFFT(wav2D : Array[Float], parameters : Array[Int], sampleLength: Int) : Array[Double] = {
    var N : Int = parameters(2)
    var FFT : Array[Double] = Array(N)
    val lastSample : Int = N / sampleLength
    var lastSampleLength : Int = N % sampleLength
    val last : Array[Complex] = FillFile(ConvertSignalToComplex(copyOfRange(wav2D, N - lastSampleLength, N - 1), lastSampleLength), sampleLength)

    for(i <- 0 to lastSample - 1) {
      var sample : Array[Complex] = new Array(N)
      
      for(j <- 0 to sampleLength)
        sample(i) = ConvertToComplex(wav2D(i * j) * hamming(i))
      
      FFT ++= ModuleFFT(sample, sampleLength)
    }
    
    FFT ++= ModuleFFT(last, sampleLength)
    return FFT
  }
  
  //Fenetre de Hamming
  def Hamming(sampleLength : Int) {
    for(i <- 0 to sampleLength)
      hamming(i) = 0.54 - 0.46 * cos((2 * Pi * i) / (sampleLength - 1))
  }
  
  //Appelle la fonction FFT avec comme argument les parties du fichier de la bonne longueur (en puissance de 2)
  def FillFile(wav2D : Array[Complex], sampleLength : Int) : Array[Complex] = {
    val wavLength = wav2D.length
    var wav : Array[Complex] = new Array(wavLength)
    wav ++= Array.fill[Complex](sampleLength - wavLength)(Complex(0, 0))
    return wav
  }
  
  //Converti chaque nombre du tableau des amplitudes du fichier en complexe
  def ConvertSignalToComplex(wav2D : Array[Float], N : Int) : Array[Complex] = {
    var wavComplex : Array[Complex] = new Array(N)
    var i : Int = 0
    while (i < N) {
      wavComplex(i) = ConvertToComplex(wav2D(i))
      i += 1
    }
    return wavComplex
  }
  
  //Comparaison binaire pour avoir la difference entre la longueur du fichier et la puissance de 2 superieure
  //Sert à remplir de 0 le tableau d'amplitude du fichier et l'avoir d'une longueur puissance de 2 pour utiliser l'algorithme de Cooley-Tuckey
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
  
  //FFT de la liste en argument, ressort une liste de frequences
  def FFTAnalysis(file : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return file
    if (N % 2 != 0) throw new RuntimeException("Problème de longueur, pas puissance de 2!")
    
    //Premiere liste de nombres d'indice pair
    var even : Array[Complex] = new Array(N / 2)
    for (k <- 0 to N / 2 - 1 by 2)
      even(k) = file(k)
    var fftEven : Array[Complex] = FFTAnalysis(even, N / 2)
    
    //Deuxieme liste de nombres d'indice impair
    var odd : Array[Complex] = even
    for (k <- 0 to N / 2 - 1 by 2)
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
  
  //Module de chaque element de la fft
  def ModuleFFT(sample : Array[Complex], N : Int) : Array[Double] = {
    var fft : Array[Complex] = FFTAnalysis(sample, N)
    var module : Array[Double] = new Array(N)
    var i : Int = 0
    while(i < N) {
      module(i) = fft(i).Module()
      i += 1
    }
    return module
  }
}