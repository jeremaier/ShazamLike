package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import FFT._

object Import {
  var filesAnalysis : Array[Array[Array[Int]]] = Array()
  var filesDirectory : Array[Array[Complex]] = Array()
  
  def WavAnalysis(file : String) : Array[Array[Int]] = {return new WavWrapper("BDD\\Je_serai.wav").getWav()} //Renvoi le son en une liste de liste de nombre
  
  def DirectoryFilesList(directoryPath : String) : Array[String] = {return Utils.listFiles(directoryPath)}   //Renvoi la liste des fichiers contenu dans la BDD
  
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {                   //Enregistre dans une liste l'ensemble des sons sous forme de listes de nombres contenus dans la BDD
    println("Dossier BDD : " + directoryPath)
    
    /*var wav : Array[Array[Int]] = WavAnalysis(directoryPath + "\\" + directoryFilesName(0))
    for (i <- 0 to wav(1).length - 1)
      println(wav(1)(i))*/
    
    for(i <- 0 to directoryFilesName.length - 1)
      filesAnalysis :+= WavAnalysis(directoryPath + "\\" + directoryFilesName(i))
      
    for (i <- 0 to filesAnalysis(0).length - 1)
      println(filesAnalysis(0)(1)(i))
    /*
    for(i <- 0 to filesAnalysis(1).length - 1)
      filesDirectory :+= FillFile(filesAnalysis(i), filesAnalysis(i)(0)(2))
    println(filesDirectory)*/
  }
}