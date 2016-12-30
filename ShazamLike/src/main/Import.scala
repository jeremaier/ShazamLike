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
  def DirectoryFilesList() : Array[Array[String]] = 
    return Array(Utils.listFiles(folders(1).getAbsolutePath).filter(! _.contains("cache.txt")),
                 Utils.listFiles(folders(2).getAbsolutePath).filter(! _.contains("cache.txt")),
                 Utils.listFiles(folders(3).getAbsolutePath).filter(! _.contains("cache.txt")))

  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes
  //Puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String, directoryNumber : Int, sampleL : Int) {
    sampleLength = sampleL
    Hamming(sampleLength)
    
    val filesNumber = directoryFilesName.length
    val filesParameters : Array[Array[String]] = new Array(filesNumber)
    val filesAmplitude : Array[Array[Double]] = new Array(filesNumber)
    //Le dossier puis le fichier puis la fft
    val filesDirectory : Array[Array[Array[Double]]] = Array.ofDim[Double](3, filesNumber, 0)
    
    var directory : String = "11"
    directoryNumber match {
      case 1 => directory = "22"
      case 2 => directory = "44"
      case _ => directory = "11"
    }
    
    for(i <- 0 to filesNumber - 1) {
      var analysis = WavAnalysis(directoryPath + "\\" + directory + "\\" + directoryFilesName(i))
      filesParameters(i) = Array(analysis(0)(0).toString(), analysis(0)(1).toString(), analysis(0)(2).toString(), directoryFilesName(i))
      var N : Int = filesParameters(i)(2).toInt
      
      if(analysis.length == 3)
        filesAmplitude(i) = StereoToMono(analysis(1), analysis(2), N)
      else filesAmplitude(i) = IntToDouble(analysis(1), N)
      
      filesDirectory(directoryNumber)(i) = SplitingAndFFT(filesAmplitude(i), filesParameters(i)(2).toInt, sampleLength)
    }
  }
  
  //Transforme une array de Int en array de Double
  def IntToDouble(ints : Array[Int], N : Int) : Array[Double] = {
    val floats : Array[Double] = new Array(N)
    for(i <- 0 to N - 1)
      floats(i) = ints(i).toFloat
    return floats
  }
  
  //Passage d'un son stereo à un son mono en faisant la moyenne des 2 canaux
  def StereoToMono(canal1 : Array[Int], canal2 : Array[Int], N : Int) : Array[Double] = {
    val stereo : Array[Double] = new Array(N)
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