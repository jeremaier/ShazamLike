package main

import scala.math._

object Fingerprinting {
  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage (le 3eme avant chaque targetzone)  
  def FingerPrint(constellation : Array[Array[Int]]) : Array[Array[Int]] = {
    val emp : Array[Array[Int]] = Array.ofDim((constellation.length - 7) * 5, 3)
    var ancrage : Int = 0
    var ecart : Int = 3
    
    for(i <- 0 to emp.length - 1) {
      if(i % 5 == 0 && i != 0) {
        ancrage += 1
        ecart -= 4
      }
      
      emp(i) = Array(constellation(ancrage)(0), constellation(ecart)(0), constellation(ecart)(1) - constellation(ancrage)(1))
      ecart += 1
    }
    
    return emp
  }
  
  def Hash(emp : Array[Int]) : Int = {
    return emp(0) * 10000000
  }
}