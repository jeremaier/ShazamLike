package main

import scala.math._
import scala.collection.mutable.Map

object Fingerprinting {
  val errorFactor : Int = 2

  //Renvoie l'empreinte d'un musique en creant des hash de groupe de 4 points et leur temps absolu dans la musique correspondant
  def FingerPrint(constellation : Array[Array[Int]]) : Array[(Long, Int)] = {
    val emp : Array[(Long, Int)] = new Array(constellation.length - 3)
    
    for(i <- 0 to emp.length - 1)
      emp(i) = (Hash(Array(constellation(i)(0), constellation(i + 1)(0), constellation(i + 2)(0), constellation(i + 3)(0))), constellation(i)(1))
    
    return emp
  }
  
  //Creation du hash à partir d'un groupe de 4 points
  def Hash(targetZone : Array[Int]) : Long = 
    return (((targetZone(3) - (targetZone(3) % errorFactor)).toLong * 100000000) + (targetZone(2) - (targetZone(2) % errorFactor)) * 100000 + (targetZone(1) - (targetZone(1) % errorFactor)) * 100 + (targetZone(0) - (targetZone(0) % errorFactor)))
}