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
  
  def FFT(file : Array[Array[Int]], l : Float) {
    //if(ham.length != 0)
    //  Hamming(N)
    //T est le tableau du fichier .wav
    //largeur de la fenêtre de Hamming
    val fe=44100 //fréquence d'échantillonnge
    val lenT:Int=T(0)(2)
    for (i <- 1 to lenT)
      {
        var S=0
        for (n <- 0 to fe)
          {
            S+=(T(i)(n))*Hamming(n-(l/2))*math.exp(-2j*Pi*fe*n)
          }
        //on rajoute |S| au carré au tableau T
      }
    return T
  }
}