package main

import scala.collection.mutable._

object Compare {
  //Renvoie l'indice de la musique la plus propice à être la meme que celle de l'echantillon
  def BestMatching(sample : Array[(Long, Int)], BDDSongs : Array[Array[(Long, Int)]]) : Int = {
    val fingerPrintsNumber : Int = BDDSongs.length
    val matchingTimes : Array[Int] = new Array[Int](fingerPrintsNumber)
    
    for(i <- 0 to fingerPrintsNumber - 1) {
      var matchingPoints : Int = FirstStep(sample, BDDSongs(i))
      
      if(matchingPoints.toFloat / sample.length >= 0.25)
        matchingTimes(i) = matchingPoints
      else matchingTimes(i) = 0
    }
    
    return MaxRating(matchingTimes)
  }
  
  //Renvoie le plus grand des nombres de cette liste qui correspond au nombre maximum d'occurence d'un deltaTime de chaque musique
  def MaxRating(matchingTimes : Array[Int]) : Int = {
    var bestMatch : Int = -1
    var maxRate : Double = 0
    
    for(i <- 0 to matchingTimes.length - 1) {
      var rate : Double = matchingTimes(i)
      
      if(rate > maxRate) {
        maxRate = rate
        bestMatch = i
      }
    }
    
    return bestMatch
  }
  
  //Compare les ensembles de points un a un et renvoie le nombre de groupes similaires et calcule les diffenrences de temps entre ceux ci
  def FirstStep(sample : Array[(Long, Int)], BDDSongs : Array[(Long, Int)]) : Int = {
    var deltaTimes : ArrayBuffer[Int] = ArrayBuffer[Int]()

    for(i <- 0 to BDDSongs.length - 1) {
      var (adressHashBDD, anchorAbsoluteTime) = BDDSongs(i)
      
      for(j <- 0 to sample.length - 1) {
        var (adressHashSample, pointAbsoluteTime) = sample(j)
        
        if(adressHashBDD == adressHashSample) {
          deltaTimes += anchorAbsoluteTime - pointAbsoluteTime
        }
      }
    }
    
    return maxDelta(deltaTimes.toArray)
  }
  
  //Renvoie le maximum d'occurences d'un nombre de cette liste
  def maxDelta(deltaTimes : Array[Int]) : Int = {
    var maxOccur : Int = 0
    
    for(i <- 1 to deltaTimes.length - 1) {
      var deltaCount : Int = deltaTimes.count(_ == deltaTimes(i))
      
      if(deltaCount > maxOccur)
        maxOccur = deltaCount      
    }
    
    return maxOccur
  }
}