package main

import scala.math._
import scala.collection.mutable.ArrayBuffer

object Compare {
  //Renvoi a quel pourcentage les musiques sont ressemblantes
  def MatchingRate(sample : Array[Double], song : Array[Double]) : (Double, ArrayBuffer[ArrayBuffer[Double]]) = {
    val t : ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer()
    var sum : Int = 0 
    var pris : Boolean = false
    var cpt : Int = 0
    
    for(i <- 0 to sample.length - 1 by 3) {
      pris = false
      
      for(j <- 0 to song.length -1 by 3) {
        if(AverageEgal(sample(i), song(j)) && AverageEgal(sample(i + 1), song(j + 1)) && !pris) {
          t append ArrayBuffer(sample(i), sample(i + 1), sample(i + 2), song(j), song(j + 1), song(j + 2))
          sum += 3
          pris = true
          cpt += 1
        } else if(AverageEgal(sample(i), song(j)) && AverageEgal(sample(i + 1), song(j + 1)) && pris)
          t(cpt - 1) append (song(j), song(j + 1), song(j + 2))
      }
    }
    
    return (sum / sample.length.toDouble, t)
  }
  
  //Renvoi si a et b sont proches de moins de 11 ou pas
  def AverageEgal(a : Double, b : Double) : Boolean = return a < b + 11 && a > b - 11
  
  //Renvoie le nombre de notes ayant le meme decalage temporel 
  def OccurencesNumber(matchingRate : ArrayBuffer[ArrayBuffer[Double]]) : Double = {
    var ref : Double = 0.0
    val count : ArrayBuffer[Double] = ArrayBuffer[Double]()
    
    for (i <- 0 to matchingRate.length - 1) {
      ref = matchingRate(i)(2)
      
      for (j <- 5 to matchingRate(i).length - 1 by 3)
        count append ref - matchingRate(i)(j)
    }
    
    return maxOc(count)
  }
  
  //nb d'occurences de la valeur la plus representee dans un tableau
  def maxOc(T:ArrayBuffer[Double]):Double = {
    var Tmax : ArrayBuffer[Double] = ArrayBuffer()
    for(i<-0 to T.length-1){
      Tmax append countDelta(T,T(i))
    }
    return maxi(Tmax)
  }
  //Prend une liste de deltas et une valeur de delta et qui ressort le nombre d'occurences de cette valeur
def countDelta(deltaTab:ArrayBuffer[Double],delta:Double):Double={
  var countDelta:Double = 0
  for (i<- 0 to deltaTab.length-1){
    if (deltaTab(i)==delta){
      countDelta+=1
    }
  }
  return countDelta
  }
//maximum d'un tableau
def maxi(Tmax:ArrayBuffer[Double]):Double={
  var occmax=Tmax(0)
  for (i<- 1 to Tmax.length -1){
    if(occmax<Tmax(i)){
      occmax=Tmax(i)
    }
  }
  return occmax
}
  
  //prend un sample et une database puis renvoie l'indice du tableau database correspondant le plus a sample
  def IndexResult(sample : Array[Double], database : Array[Array[Double]]) : Int = {
    val id : ArrayBuffer[Int] = ArrayBuffer[Int]()
    val nbOccu : ArrayBuffer[Double] = ArrayBuffer[Double]()
    
    for(i <- 0 to database.length - 1) {
      var taux = MatchingRate(sample, database(i))._1
      
      if(taux >= 0.8)
        id append i
    }
    
    if (id.length == 0)
      return -1
    
    for(j <- 0 to id.length - 1)
      nbOccu append OccurencesNumber(MatchingRate(sample, database(j))._2)

    return id(IndexMax(nbOccu))
  }
  
  //Prend un tableau et renvoi l'indice du maximum de la liste
  def IndexMax(T : ArrayBuffer[Double]) : Int = {
    var indice : Int = 0
    
    for(i <- 1 to T.length - 1) {
      if(T(i) > T(indice))
        indice = i
    }
    
    return indice
  }
}