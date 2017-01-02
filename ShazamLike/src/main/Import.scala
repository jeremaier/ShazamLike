package main

import com.tncy.top.files.WavWrapper
import com.tncy.top.files.Utils
import Array._
import BDD._
import FFT._
import Fingerprinting.FingerPrint
import Constellation.Spectrogram
import GUI._

object Import {
  val sampleLength : Int = 1024
  val frequency : Int = 11025
  var fingerPrintsDirectory : Array[Array[Array[Double]]] = Array[Array[Array[Double]]]()
  var filesNames : Array[Array[String]] = Array[Array[String]]()
  
  //Renvoi le son en une liste de liste de nombres representant les amplitudes en fonction du temps
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()
  
  //Renvoi la liste des noms des fichiers contenu dans la BDD
  def DirectoryFilesList() : Array[Array[String]] = return Array(Utils.listFiles(folders(1).getAbsolutePath).filter(! _.contains("cache.txt")))

  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes
  //Puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {    
    val filesNumber = directoryFilesName.length
    val filesParameters : Array[Array[Int]] = new Array[Array[Int]](filesNumber)
    val filesAmplitude : Array[Array[Double]] = new Array[Array[Double]](filesNumber)
    fingerPrintsDirectory = Array.ofDim[Double](3, filesNumber, 0)
    filesNames = Array.ofDim[String](3, filesNumber)
    
    for(i <- 0 to filesNumber - 1) {
      var analysis = WavAnalysis(directoryPath + "\\" + directory + "\\" + directoryFilesName(i))
      filesParameters(i) = analysis(0)
      filesNames(directoryNumber)(i) = directoryFilesName(i)
      var N : Int = 0
      var freq : Int = filesParameters(i)(0)
      
      if(analysis.length == 3) {
        N = analysis(1).length
        
        if(freq != 11025)
          filesAmplitude(i) = StereoToMono(analysis(1), analysis(2), N)
        else filesAmplitude(i) = DownSampling(StereoToMono(analysis(1), analysis(2), N), N, freq)
      } else {
        N = filesParameters(i)(2)
        
        if(freq != 11025)
          filesAmplitude(i) = DownSampling(analysis(1), N, freq)
        else filesAmplitude(i) = IntToDouble(analysis(1), N)
      }
      
      fingerPrintsDirectory(directoryNumber)(i) = FingerPrint(Spectrogram(SplitingAndFFT(filesAmplitude(i), filesParameters(i)(2), sampleLength), sampleLength, frequency))
    }
    
    CacheWriter(directoryNumber + 1, directoryFilesName, fingerPrintsDirectory(directoryNumber))
	  dateCacheWrite(getDate())
	  SetReadyBDD(true)
  }
  
  //DownSampling
  def DownSampling(wav : Array[Int], length : Int, freq : Int) : Array[Double] = {
    if(freq == 44100)
      return DownSampling44(wav, length)
    else return DownSampling22(wav, length)
  }
  
  //Passage de la frequence d'echantillonage de 22050 a 11025
  def DownSampling22(wav : Array[Int], length : Int) : Array[Double] = {
    val wav11 : Array[Double] = new Array(length / 2)
    for(i <- 0 to length - 1 by 2)
      wav11(i / 2) = wav(i).toDouble
    return wav11
  }
  
  //Passage de la frequence d'echantillonage de 44100 a 22050
  def DownSampling44(wav : Array[Int], length : Int) : Array[Double] = return DownSampling22(DownSampling22(wav, length), length / 2)
  
  //Transforme une array de Int en array de Double
  def IntToDouble(ints : Array[Int], N : Int) : Array[Double] = {
    val floats : Array[Double] = new Array[Double](N)
    for(i <- 0 to N - 1)
      floats(i) = ints(i).toDouble
    return floats
  }
  
  //Passage d'un son stereo à un son mono en faisant la moyenne des 2 canaux
  def StereoToMono(canal1 : Array[Int], canal2 : Array[Int], N : Int) : Array[Double] = {
    val stereo : Array[Double] = new Array[Double](N)
    
    println(stereo.length, canal1.length, canal2.length)
    for(i <- 0 to N - 1)
    	stereo(i) = (canal1(i) + canal2(i)).toDouble / 2
    	
    return stereo
  }
  
  def SetSampleLength(sampleL : Int) {sampleLength = sampleL}
}