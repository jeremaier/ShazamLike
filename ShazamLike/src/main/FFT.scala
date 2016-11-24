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
  
  def FFT(file : Array[Array[Int]]) {
    //if(ham.length != 0)
    //  Hamming(N)
  }
  
  def Hamming(N : Int) {
    for (n <- 0 to N - 1)
      ham :+ (0.54 - 0.46 * cos(2 * Pi * n / 50))
  }
}