package main

import scala.math._

object Fingerprinting {
  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage (le 3eme avant chaque targetzone)  
  def FingerPrint(constellation : Array[Array[Double]]) : Array[Double] = {
    val emp : Array[Double] = new Array[Double]((constellation.length - 7) * 15)
    var ancrage : Int = 0
    var ecart : Int = 3
    
    for(i <- 0 to emp.length - 1 by 3) {
      if(i % 5 == 0 && i != 0) {
        ancrage += 1
        ecart -= 4
      }
      
      emp(i) = constellation(ancrage)(0)
      emp(i + 1) = constellation(ecart)(0)
      emp(i + 2) = constellation(ecart)(1) - constellation(ancrage)(1)

      ecart += 1
    }
    
    return emp
  }
}