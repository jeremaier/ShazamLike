package main

import scala.math._
import scala.collection.mutable.Map

object Fingerprinting {
  val errorFactor : Int = 2

  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage (le 3eme avant chaque targetzone)
  def FingerPrint(constellation : Array[Array[Int]]) : Array[(Long, Int)] = {
    val emp : Array[(Long, Int)] = new Array(constellation.length - 3)
    
    for(i <- 0 to emp.length - 1)
      emp(i) = (Hash(Array(constellation(i)(0), constellation(i + 1)(0), constellation(i + 2)(0), constellation(i + 3)(0))), constellation(i)(1))
    
    return emp
  }
  
  //Creation du hash à partir des frequences et deltaTime
  def Hash(targetZone : Array[Int]) : Long = 
    return (((targetZone(3) - (targetZone(3) % errorFactor)).toLong * 100000000) + (targetZone(2) - (targetZone(2) % errorFactor)) * 100000 + (targetZone(1) - (targetZone(1) % errorFactor)) * 100 + (targetZone(0) - (targetZone(0) % errorFactor)))
}