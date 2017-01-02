package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import BDD._
import FFT._
import Fingerprinting.FingerPrint
import Constellation.Spectrogram
import GUI.RefreshReadyBDD

object Import {
  var sampleLength : Int = 1024
  var fingerPrintsDirectory : Array[Array[Array[Double]]] = Array[Array[Array[Double]]]()
  var filesNames : Array[Array[String]] = Array[Array[String]]()
  
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
    SetSampleLength(sampleL)
    Hamming(sampleLength)
    
    val filesNumber = directoryFilesName.length
    val filesParameters : Array[Array[Int]] = new Array[Array[Int]](filesNumber)
    val filesAmplitude : Array[Array[Double]] = new Array[Array[Double]](filesNumber)
    //Le dossier puis le fichier puis la fft
    fingerPrintsDirectory = Array.ofDim[Double](3, filesNumber, 0)
    filesNames = Array.ofDim[String](3, filesNumber)
    
    var directory : String = "11"
    directoryNumber match {
      case 1 => directory = "22"
      case 2 => directory = "44"
      case _ => directory = "11"
    }
    
    for(i <- 0 to filesNumber - 1) {
      var analysis = WavAnalysis(directoryPath + "\\" + directory + "\\" + directoryFilesName(i))
      filesParameters(i) = analysis(0)
      filesNames(directoryNumber)(i) = directoryFilesName(i)
      var N : Int = 0
      
      if(analysis.length == 3) {
        N = analysis(1).length
        filesAmplitude(i) = StereoToMono(analysis(1), analysis(2), N)
      } else {
        N = filesParameters(i)(2)
        filesAmplitude(i) = IntToDouble(analysis(1), N)
      }
      
      fingerPrintsDirectory(directoryNumber)(i) = FingerPrint(Spectrogram(SplitingAndFFT(filesAmplitude(i), filesParameters(i)(2), sampleLength), sampleLength, filesParameters(i)(0)))
    }
    
    CacheWriter(directoryNumber + 1, directoryFilesName, fingerPrintsDirectory(directoryNumber))
	  dateCacheWrite(getDate())
	  RefreshReadyBDD(true, false)
  }
  
  //Transforme une array de Int en array de Double
  def IntToDouble(ints : Array[Int], N : Int) : Array[Double] = {
    val floats : Array[Double] = new Array[Double](N)
    for(i <- 0 to N - 1)
      floats(i) = ints(i).toFloat
    return floats
  }
  
  //Passage d'un son stereo à un son mono en faisant la moyenne des 2 canaux
  def StereoToMono(canal1 : Array[Int], canal2 : Array[Int], N : Int) : Array[Double] = {
    val stereo : Array[Double] = new Array[Double](N)
    
    println(stereo.length, canal1.length, canal2.length)
    for(i <- 0 to N - 1)
    	stereo(i) = (canal1(i) + canal2(i)).toFloat / 2
    	
    return stereo
  }
  
  def SetSampleLength(sampleL : Int) {sampleLength = sampleL}
}