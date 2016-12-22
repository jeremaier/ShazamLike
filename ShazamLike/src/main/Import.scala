package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import BDD.folders
import FFT._

object Import {
  var sampleLength : Int = 1024
  
  //Renvoi le son en une liste de liste de nombres representant les amplitudes en fonction du temps
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()
  
  //Renvoi la liste des noms des fichiers contenu dans la BDD
  def DirectoryFilesList() : Array[Array[String]] = return Array(Utils.listFiles(folders(0).getAbsolutePath), Utils.listFiles(folders(1).getAbsolutePath), Utils.listFiles(folders(2).getAbsolutePath))

  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes
  //Puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {
    println("Dossier BDD : " + directoryPath)
    
    Hamming(sampleLength)
    
    val filesNumber = directoryFilesName.length
    val filesParameters : Array[Array[Int]] = new Array(filesNumber)
    val filesAmplitude : Array[Array[Float]] = new Array(filesNumber)
    val filesDirectory : Array[Array[Double]] = new Array(filesNumber)
    
    for(i <- 0 to filesNumber - 1) {
      var analysis = WavAnalysis(directoryPath + "\\" + directoryFilesName(i))
      filesParameters(i) = analysis(0)
      var N : Int = filesParameters(i)(2)
      
      if(analysis.length == 3)
        filesAmplitude(i) = StereoToMono(analysis(1), analysis(2), N)
      else filesAmplitude(i) = IntToFloat(analysis(1), N)
      
      filesDirectory(i) = SplitingAndFFT(filesAmplitude(i), filesParameters(i), sampleLength)
    }
  }
  
  //Transforme une array de Int en array de Double
  def IntToFloat(ints : Array[Int], N : Int) : Array[Float] = {
    val floats : Array[Float] = new Array(N)
    for(i <- 0 to N - 1)
      floats(i) = ints(i).toFloat
    return floats
  }
  
  //Passage d'un son stereo à un son mono en faisant la moyenne des 2 canaux
  def StereoToMono(canal1 : Array[Int], canal2 : Array[Int], N : Int) : Array[Float] = {
    val stereo : Array[Float] = new Array(N)
    var i : Int = 0
    while (i < N) {
      stereo(i) = (canal1(i) + canal2(i)).toFloat / 2
      i += 1
    }
    return stereo
    /*
    for(i <- 0 to N - 1)
    	stereo(i) = (canal1(i) + canal2(i)).toFloat / 2
    */
  }
}