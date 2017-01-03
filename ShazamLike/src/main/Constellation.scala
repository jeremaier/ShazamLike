package main

import scala.collection.mutable._

object Constellation {
  //Tableau de la forme[A, A, A, A, A,...]
  //Les frequences sont k * frequences d'echantillonages / sampleLength
  //Le temps de ces frequences sont la taille des fenetres selectionnes * 0.1 seconde
  def Spectrogram(frequencyAmplitudes : Array[Double], sampleLength : Int, samplingFrequency : Int) : Array[Array[Double]] =
    return SupMean(Max(frequencyAmplitudes, BandsGeneration(sampleLength), samplingFrequency), Mean(frequencyAmplitudes))
  
  //Generation des limites de bandes de frequences
  def BandsGeneration(limit : Int) : Array[Int] = {
    val bands : Array[Int] = new Array[Int](7)
    var sep : Int = 10 * limit / 1024
    
    bands(0) = 0

    for(i <- 1 to 5) {
      bands(i) = sep
      sep *= 2
    }
    
    bands(6) = limit / 2 - 1
    
    return bands
  }

  //Renvoi le tableau des amplitudes les plus hautes avec la frequence correspondante
  def Max(frequencyAmplitudes : Array[Double], bandes : Array[Int], samplingFrequency : Int) : Array[Array[Double]] = {
    val sampleLength : Int = (bandes(6) + 1) * 2
    val samples : Int = frequencyAmplitudes.length / sampleLength
    val tabMax : Array[Array[Double]] = new Array[Array[Double]](6 * samples)
    
    for(i <- 0 to samples - 1) {
      var firstIndex : Int = i * sampleLength
              
      for(j <- 0 to bandes.length - 2) {
        var max : Double = frequencyAmplitudes(firstIndex)
        var index : Int = 0
        
        for(k <- bandes(j) + 1 to bandes(j + 1)) {
          var l = firstIndex + k
          
          if (frequencyAmplitudes(l) > max) {
            max = frequencyAmplitudes(l)
            index = l - sampleLength * i
          }
        }
        
        tabMax(i * 6 + j) = Array[Double](max, index * samplingFrequency.toFloat / sampleLength)
      }
    }
    
    return tabMax
  }
  
  //Renvoi la moyenne des amplitudes frequentielles
  def Mean(frequencyAmplitudes : Array[Double]) : Double = {
    var sum : Double = 0
    val length : Int = frequencyAmplitudes.length
    
    for (i <- 0 to length - 1)
      sum += frequencyAmplitudes(i)
    
    return sum / length
  }
  
  //Retourne le tableau des frequences, et le temps qui correspond, des amplitudes frequentielles au dessus de la moyenne 
  def SupMean(maxArray : Array[Array[Double]], mean : Double) : Array[Array[Double]] = {
    val supArray : ArrayBuffer[Array[Double]] = ArrayBuffer[Array[Double]]()
    
    for(i <- 0 to maxArray.length - 1) {
      if(maxArray(i)(0) >= mean * 2)
        supArray += Array[Double](maxArray(i)(1), i / 6)
    }
            
    return supArray.toArray
  }
}