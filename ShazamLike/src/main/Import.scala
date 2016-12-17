package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import FFT._

object Import {
  var filesParameters : Array[Array[Int]] = Array()
  var filesAmplitude : Array[Array[Int]] = Array()
  var filesDirectory : Array[Array[Double]] = Array()
  
  //Renvoi le son en une liste de liste de nombres representant les amplitudes en fonction du temps
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()
  
  //Renvoi la liste des noms des fichiers contenu dans la BDD
  def DirectoryFilesList(directoryPath : String) : Array[String] = return Utils.listFiles(directoryPath)
  
  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes
  //Puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {
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