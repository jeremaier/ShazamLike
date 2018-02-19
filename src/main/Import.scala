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
  var fingerPrintsDirectory : Array[Array[(Long, Int)]] = Array[Array[(Long, Int)]]()
  var filesNames : Array[String] = Array[String]()
  
  //Renvoie le son en une liste de listes de nombres representant les amplitudes en fonction du temps
  def WavAnalysis(file : String) : Array[Array[Int]] = return new WavWrapper(file).getWav()
  
  //Renvoie la liste des noms des fichiers contenus dans la BDD
  def DirectoryFilesList() : Array[String] = return Utils.listFiles(folder.getAbsolutePath).filter(! _.contains(".txt"))
  
  //Enregistre dans 2 listes differentes les parametres de chaque son et les listes des amplitudes puis lancement des FFT
  def DirectoryFilesAnalysis(directoryFilesName : Array[String], directoryPath : String) {
    val filesNumber : Int = directoryFilesName.length
    var file : Array[Double] = Array[Double]()
    fingerPrintsDirectory = new Array[Array[(Long, Int)]](filesNumber)
    filesNames = new Array[String](filesNumber)
    Hamming(sampleLength)
    
    for(i <- 0 to filesNumber - 1) {
      println(filesNumber - i)
      filesNames(i) = directoryFilesName(i)
      file = DownSamplingAndStereoToMono(WavAnalysis(directoryPath + "\\" + directoryFilesName(i)))
      fingerPrintsDirectory(i) = FingerPrint(Spectrogram(SplitingAndFFT(file, file.length, sampleLength), frequencyBase, sampleLength))
    }
    
    CacheWriter(directoryFilesName, fingerPrintsDirectory)
	  DateCacheWrite(folder.lastModified().toString())
	  SetReadyBDD(true)
	  println("Création des empreintes terminée en : " + (System.currentTimeMillis() - timeAnalysis) / 1000F)
  }
  
  //Passage de la frequence d'echantillonage de 22050 a 11025
  def DownSampling22(wav : Array[Double], length : Int) : Array[Double] = {
    val wav11 : Array[Double] = new Array(length / 2)
    
    for(i <- 0 to length - 1 by 2)
      wav11(i / 2) = wav(i)
    
    return wav11
  }
  
  //Passage de la frequence d'echantillonage de 44100 a 22050
  def DownSampling44(wav : Array[Double], length : Int) : Array[Double] = return DownSampling22(DownSampling22(wav, length), length / 2)
  
  //Passe n'importe quel type de musique en musique mono de frequence d'echantillonnage 11025Hz
  def DownSamplingAndStereoToMono(wav : Array[Array[Int]]) : Array[Double] = {
    var parameters : Array[Int] = wav(0)
    var frequency : Int = parameters(0)
    var channels : Int = parameters(1)
    var N : Int = parameters(2)
    var doubleWav : Array[Double] = Array()
    
    if(channels == 2) 
      doubleWav = StereoToMono(wav(1), wav(2), N)
    else doubleWav = IntToDouble(wav(1), N)
    
    if(frequency == 44100)
      return DownSampling44(doubleWav, N)
    else if(frequency == 22050)
      return DownSampling22(doubleWav, N)
    else return doubleWav
  }
  
  //Transforme une lsite de Int en liste de Double
  def IntToDouble(ints : Array[Int], N : Int) : Array[Double] = {
    val floats : Array[Double] = new Array[Double](N)
    
    for(i <- 0 to N - 1)
      floats(i) = ints(i).toDouble
    
    return floats
  }
  
  //Passage d'un son stereo à un son mono en faisant la moyenne des 2 canaux contenant les amplitudes frequencielles
  def StereoToMono(canal1 : Array[Int], canal2 : Array[Int], N : Int) : Array[Double] = {
    val stereoFrequencies : Array[Double] = new Array[Double](N)
    
    for(i <- 0 to N - 1)
    	stereoFrequencies(i) = (canal1(i) + canal2(i)) / 2
    	
    return stereoFrequencies
  }
}