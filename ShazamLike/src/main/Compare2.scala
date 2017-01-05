package main

import Import._

object Compare2 {
  def FirstMatchingRate(sample : Array[Array[Double]], song : Array[Array[Double]]) : Double = {
    var matchingTargetNumber : Int = 0
    val sampleLength : Int = sample.length
    val songLength : Int = song.length
    
    for(i <- 0 to songLength - 1) {
      for(j <- 0 to sampleLength - 1) {
        if(song(i) == sample(j))
          matchingTargetNumber += 1
      }
    }
    
    return matchingTargetNumber / sampleLength
  }
  
  def SecondMatchingRate(sample : Array[Array[Double]], song : Array[Array[Double]]) : Double = {
    var matchingTarget : Int = 0
    
    
    return matchingTarget / (song.length * 5)
  }
  
  def BestMatchingRate(sample : Array[Array[Double]], fingerPrints : Array[Array[Array[Double]]]) : Int = {
    var bestMatch : Int = -1
    val fingerPrintsNumber : Int = fingerPrints.length
    val matchingRates : Array[Double] = new Array[Double](fingerPrintsNumber)
    
    for(i <- 0 to fingerPrintsNumber - 1) {
      var rate : Double = FirstMatchingRate(sample, fingerPrints(i))
      
      if(rate < 0.8)
        matchingRates(i) = 0
      else if(rate >= 0.8 && rate <= 1)
        matchingRates(i) = rate
      else matchingRates(i) = 1.0
    }
    
    for(i <- 0 to fingerPrints.length - 1) {
      if(matchingRates(i) != 0) {
        var rate : Double = SecondMatchingRate(sample, fingerPrints(i))
        
        if(rate > 0.8)
          matchingRates(i) = 0
        else matchingRates(i) = rate
      }
    }
    
    var maxRate : Double = 0
    
    for(i <- 0 to matchingRates.length - 1) {
      var rate : Double = matchingRates(i)
      
      if(rate > maxRate) {
        maxRate = rate
        bestMatch = i
      }
    }
    
    return bestMatch
  }
}