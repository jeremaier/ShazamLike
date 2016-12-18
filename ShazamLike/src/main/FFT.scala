package main

import scala.math._
import Array._
import Complex._
import Import._

object FFT {  
  //Appelle la fonction FFT avec comme argument les parties du fichier de la bonne longueur (en puissance de 2)
  def FillFile(wav2D : Array[Float], n : Int) : Array[Complex] = {
    var wav : Array[Complex] = ConvertSignalToComplex(wav2D, n)
    var sup2 = ArrayLength(wav.length)
    wav ++= Array.fill[Complex](sup2 - n)(Complex(0, 0))
    return FFTAnalysis(wav, sup2)
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
  def ModuleFFT(fft : Array[Complex], N : Int) : Array[Double] = {
    var module : Array[Double] = new Array(N)
    var i : Int = 0
    while(i < N) {
      module(i) = fft(i).Module()
      i += 1
    }
    return module
  }
}