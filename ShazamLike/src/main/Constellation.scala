package main

import scala.collection.mutable._

object Constellation {
  //Renvoie le tableau des amplitudes les plus hautes et sa frequence correspondante d'une musique
  def Spectrogram(frequencyAmplitudes : Array[Double], samplingFrequency : Int, sampleLength : Int) : Array[Array[Int]] = {
    val bands : Array[Int] = BandsGeneration(sampleLength)
    val samples : Int = frequencyAmplitudes.length / sampleLength
    val supArray : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()
    val mean : Double = Mean(frequencyAmplitudes)
    val coeffMean : Double = 2
    
    for(i <- 0 to samples - 1) {
      var firstIndex : Int = i * sampleLength
              
      for(j <- 0 to bands.length - 2) {
        var max : Double = frequencyAmplitudes(firstIndex)
        var index : Int = 0
        
        for(k <- bands(j) + 1 to bands(j + 1)) {
          var l = firstIndex + k
          
          if (frequencyAmplitudes(l) > max) {
            max = frequencyAmplitudes(l)
            index = l - sampleLength * i
          }
        }
        
        if(max >= mean * coeffMean)
          supArray += Array[Int](index, i)
      }
    }
    
    return supArray.toArray
  }
  
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
  
  //Renvoie la moyenne des amplitudes frequentielles de l'ensemble du son
  def Mean(frequencyAmplitudes : Array[Double]) : Double = {
    var sum : Double = 0
    val length : Int = frequencyAmplitudes.length
    
    for (i <- 0 to length - 1)
      sum += frequencyAmplitudes(i)
    
    return sum / length
  }
}