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
  val frequencyBase : Int = 11025
  var fingerPrintsDirectory : Array[Array[Double]] = Array[Array[Double]]()
  var filesNames : Array[String] = Array[String]()
  
  //Renvoi le son en une liste de liste de nombres representant les amplitudes en fonction du temps
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()
  
  //Renvoi la liste des noms des fichiers contenu dans la BDD
  def DirectoryFilesList() : Array[String] = return Utils.listFiles(folder.getAbsolutePath).filter(! _.contains(".txt"))
  
  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes
  //Puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {    
    val filesNumber : Int = directoryFilesName.length
    var file : Array[Double] = Array[Double]()
    fingerPrintsDirectory = new Array[Array[Double]](filesNumber)
    filesNames = new Array[String](filesNumber)
    Hamming(sampleLength)
    
    for(i <- 0 to filesNumber - 1) {
      filesNames(i) = directoryFilesName(i)
      file = DownSamplingAndStereoToMono(WavAnalysis(directoryPath + "\\" + directoryFilesName(i)))
      fingerPrintsDirectory(i) = FingerPrint(Spectrogram(SplitingAndFFT(file, file.length, sampleLength), sampleLength, frequencyBase))
    }
    
    CacheWriter(directoryFilesName, fingerPrintsDirectory)
	  dateCacheWrite(folder.lastModified().toString())
	  SetReadyBDD(true)
  }
  
  //Passage de la frequence d'echantillonage de 22050 a 11025 pour du Mono
  def DownSampling22(wav : Array[Int], length : Int) : Array[Int] = {
    val wav11 : Array[Int] = new Array(length / 2)
    for(i <- 0 to length - 1 by 2)
      wav11(i / 2) = wav(i)
    return wav11
  }
  
  //Passage de la frequence d'echantillonage de 44100 a 22050 pour du Mono
  def DownSampling44(wav : Array[Int], length : Int) : Array[Int] = return DownSampling22(DownSampling22(wav, length), length / 2)
  
  //Passage de la frequence d'echantillonage de 22050 a 11025 pour du Stereo
  def DownSampling22Stereo(wav : Array[Double], length : Int) : Array[Double] = {
    val wav11 : Array[Double] = new Array(length / 2)
    for(i <- 0 to length - 1 by 2)
      wav11(i / 2) = wav(i)
    return wav11
  }
  
  //Passage de la frequence d'echantillonage de 44100 a 22050 pour du Stereo
  def DownSampling44Stereo(wav : Array[Double], length : Int) : Array[Double] = return DownSampling22Stereo(DownSampling22Stereo(wav, length), length / 2)
  
  def DownSamplingAndStereoToMono(wav : Array[Array[Int]]) : Array[Double] = {
    var parameters : Array[Int] = wav(0)
    var frequency : Int = parameters(0)
    var channels : Int = parameters(1)
    var N : Int = parameters(2)
    
    if(channels == 2) {
      if(frequency == 44100)
        return DownSampling44Stereo(StereoToMono(wav(1), wav(2), N), N)
      else if(frequency == 22050)
        return DownSampling22Stereo(StereoToMono(wav(1), wav(2), N), N)
      else return StereoToMono(wav(1), wav(2), N)
    } else {
      if(frequency == 44100)
        return IntToDouble(DownSampling44(wav(1), N), N / 4)
      else if(frequency == 22050)
        return IntToDouble(DownSampling22(wav(1), N), N / 2)
      else return IntToDouble(wav(1), N)
    }
  }
  
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
    
    for(i <- 0 to N - 1)
    	stereo(i) = canal1(i)
    	
    return stereo
  }
}