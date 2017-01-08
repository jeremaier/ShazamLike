package main

import scala.collection.mutable._

object Compare {
  def FirstStep(sample : Array[(Long, Int)], BDDSongs : Array[(Long, Int)]) : (Int, Int) = {
    var matchingAdress : Int = 0
    var deltaTimes : ArrayBuffer[Int] = ArrayBuffer[Int]()

    for(i <- 0 to BDDSongs.length - 1) {
      var (adressHashBDD, anchorAbsoluteTime) = BDDSongs(i)
      
      for(j <- 0 to sample.length - 1) {
        var (adressHashSample, pointAbsoluteTime) = sample(j)
        
        if(adressHashBDD == adressHashSample) {
          matchingAdress += 1
          deltaTimes += anchorAbsoluteTime - pointAbsoluteTime
        }
      }
    }
    
    return (matchingAdress, maxDelta(deltaTimes.toArray))
  }
  
  def maxDelta(deltaTimes : Array[Int]) : Int = {
    var maxOccur : Int = 0
    
    for(i <- 1 to deltaTimes.length - 1) {
      var deltaCount : Int = deltaTimes.count(_ == deltaTimes(i))
      
      if(deltaCount > maxOccur)
        maxOccur = deltaCount      
    }
    
    return maxOccur
  }
  
  def BestMatching(sample : Array[(Long, Int)], BDDSongs : Array[Array[(Long, Int)]]) : Int = {
    var bestMatch : Int = -1
    val fingerPrintsNumber : Int = BDDSongs.length
    val matchingTimes : Array[Double] = new Array[Double](fingerPrintsNumber)
    
    for(i <- 0 to fingerPrintsNumber - 1) {
      var matchingPoints : (Int, Int) = FirstStep(sample, BDDSongs(i))
      
      if(matchingPoints._1 / sample.length >= 0.8)
        matchingTimes(i) = matchingPoints._2
      else matchingTimes(i) = 0
    }
    
    var maxRate : Double = 0
    
    for(i <- 0 to matchingTimes.length - 1) {
      var rate : Double = matchingTimes(i)
      
      if(rate > maxRate) {
        maxRate = rate
        bestMatch = i
      }
    }
    
    println(matchingTimes(0))
    
    return bestMatch
  }
  
  def ConvertToMap(list : Array[Int]) : Map[Int,Int] = {
    var hashTable : Map[Int,Int] = Map()
    
    for(i <- list)
      hashTable += (i -> 0)
    
    return hashTable
  }
}