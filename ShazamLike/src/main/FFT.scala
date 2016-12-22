package main

import scala.math._
import Array._
import java.util.Arrays.copyOfRange
import Complex._
import Import._

object FFT {
  var hamming : Array[Double] = new Array(sampleLength)
  
  //Decoupe le son à analyser en des samples de 4096 amplitudes et effectue la FFT sur chacune d'elle après avoir appliqué hamming pour retourner la liste de toutes ces FFT
  def SplitingAndFFT(wav2D : Array[Float], parameters : Array[Int], sampleLength: Int) : Array[Double] = {
    val N : Int = parameters(2)
    val samples : Int = N / sampleLength
    val lastSampleLength : Int = N % sampleLength
    val last : Array[Complex] = FillFile(ConvertSignalToComplex(copyOfRange(wav2D, N - lastSampleLength, N), lastSampleLength), sampleLength)
    var FFT : Array[Double] = Array()
    
    for(i <- 0 to samples - 1) {
      var sample : Array[Complex] = new Array(sampleLength)
      
      for(j <- 0 to sampleLength - 1)
        sample(j) = ConvertToComplex(wav2D(sampleLength * i + j) * hamming(j))
      
      FFT ++= ModuleFFT(sample, sampleLength)
    }
    
    FFT ++= ModuleFFT(last, sampleLength)
    return FFT
  }
  
  /*
  //On fait la moyenne de 2 samples pour ramener tous les sons a 11025Hz -> reduction du nombre de calculs + possibilite de comparer les sons de differentes frequences
  def DownSamplingTo11(sample1 : Array[Float], sample2 : Array[Float], sampleLength : Int) : Array[Float] = {
    val sample : Array[Float] = Array()
    for(i <- 0 to sampleLength)
      sample(i) = (sample1(i) + sample2(i)) / 2
    return sample
  }
  
  //Moyenne de samples 
  def DownSamplingTo11(sample1 : Array[Float], sample2 : Array[Float], sample3 : Array[Float], sample4 : Array[Float], sampleLength : Int) : Array[Float] = 
    return DownSamplingTo11(DownSamplingTo11(sample1, sample2, sampleLength), DownSamplingTo11(sample3, sample4, sampleLength), sampleLength)
  */
    
  //Fenetre de Hamming
  def Hamming(sampleLength : Int) {
    for(i <- 0 to sampleLength - 1)
      hamming(i) = 0.54 - 0.46 * cos((2 * Pi * i) / (sampleLength - 1))
  }
  
  //Appelle la fonction FFT avec comme argument les parties du fichier de la bonne longueur (en puissance de 2)
  def FillFile(last : Array[Complex], sampleLength : Int) : Array[Complex] = {
    val wavLength = last.length
    var wav : Array[Complex] = new Array(sampleLength)
    wav = last ++ Array.fill[Complex](sampleLength - wavLength)(Complex(0, 0))
    return wav
  }
  
  //Converti chaque nombre du tableau des amplitudes du fichier en complexe
  def ConvertSignalToComplex(wav2D : Array[Float], N : Int) : Array[Complex] = {
    val wavComplex : Array[Complex] = new Array(N)
    for(i <- 0 to N - 1)
      wavComplex(i) = ConvertToComplex(wav2D(i))
    return wavComplex
  }
  
  //Comparaison binaire pour avoir la difference entre la longueur du fichier et la puissance de 2 superieure
  //Sert à remplir de 0 le tableau d'amplitude du fichier et l'avoir d'une longueur puissance de 2 pour utiliser l'algorithme de Cooley-Tuckey
  /*def ArrayLength(length : Int) : Int = {
    var N : Int = length
    var n : Int = N
    if((n & (N - 1)) != 0)
      do {
        N = n
        n &= (n - 1)
      } while(n != 0)
    return N - n << 1
  }*/
  
  //FFT d'un sample, ressort une liste de frequences
  def FFTAnalysis(sample : Array[Complex], N : Int) : Array[Complex] = {
    if (N == 1) return sample
    
    //Premiere liste de nombres d'indice pair
    val even : Array[Complex] = new Array(N / 2)
    for (k <- 0 to (N - 1) / 2)
      even(k) = sample(2 * k)
    val fftEven : Array[Complex] = FFTAnalysis(even, N / 2)
    
    //Deuxieme liste de nombres d'indice impair
    val odd : Array[Complex] = new Array(N / 2)
    for (k <- 0 to (N - 1) / 2)
      odd(k) = sample(2 * k + 1)
    val fftOdd : Array[Complex] = FFTAnalysis(odd, N / 2)
    
    //Calcul de la fft par la methode de Cooley-Tuckey
    var fft : Array[Complex] = new Array(N)
    for (k <- 0 to N / 2 - 1) {
      var kth : Double = - 2 * k * Pi / N
      var wk : Complex = new Complex(cos(kth), sin(kth))
      fft(k) = fftEven(k) + (wk * fftOdd(k))
      fft(k + N / 2) = fftEven(k) + -(wk * fftOdd(k))
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