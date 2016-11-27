package main

import com.tncy.top.files.WavWrapper;
import com.tncy.top.files.Utils;
import Array._

object Import {  
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()              //Renvoi le son en une liste de liste de nombre
  
  def DirectoryFilesList(directoryPath : String) : Array[String] = return Utils.listFiles(directoryPath) //Renvoi la liste des fichiers contenu dans la BDD
  
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {               //Enregistre dans une liste l'ensemble des sons sous forme de listes de nombres contenus dans la BDD
    println("Dossier BDD : " + directoryPath)
    var filesAnalysis : Array[Array[Int]] = Array();
    
    for(i <- 0 to directoryFilesName.length)
      filesAnalysis :+ WavAnalysis(directoryPath + "\\" + directoryFilesName(i))
    
    FFT.DirectoryFilesFFT(filesAnalysis(1), filesAnalysis(0)(2))
  }
}