package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import FFT._

object Import {
  var filesParameters : Array[Array[Int]] = Array()
  var filesAmplitude : Array[Array[Int]] = Array()
  var filesDirectory : Array[Array[Double]] = Array()
  
  def WavAnalysis(file : String) : Array[Array[Int]] = {return new WavWrapper(file).getWav()} //Renvoi le son en une liste de liste de nombre
  
  def DirectoryFilesList(directoryPath : String) : Array[String] = {return Utils.listFiles(directoryPath)}   //Renvoi la liste des fichiers contenu dans la BDD
  
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {                   //Enregistre dans une liste l'ensemble des sons sous forme de listes de nombres contenus dans la BDD
    println("Dossier BDD : " + directoryPath)
    
    for(i <- 0 to directoryFilesName.length - 1) {
      filesParameters :+= WavAnalysis(directoryPath + "\\" + directoryFilesName(i))(0)
      filesAmplitude :+= WavAnalysis(directoryPath + "\\" + directoryFilesName(i))(1)
    }
    
    for(i <- 0 to filesAmplitude.length - 1) {
      var N : Int = filesParameters(i)(2)
      filesDirectory :+= ModuleFFT(FillFile(filesAmplitude(i), N), N)
    }
  }
}