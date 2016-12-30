package main

import scala.math._

object Fingerprinting {
  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage
  def resume(T : Array[Array[Double]]) : Array[Array[Double]] = return empreinte(T)
  
  //On prend le 6eme point de la target zone et le point juste avant cette target zone pour faire une empreinte
  def empreinte(T : Array[Array[Double]]) : Array[Array[Double]] = {
    var emp : Array[Array[Double]] = new Array[Array[Double]](T.length - 7)
    
    for(i <- 0 to emp.length - 1) {
      var firstPointIndex : Int = i + 3
      
      for(j <- 0 to 4)
        emp(j) = Array[Double](T(i)(0), T(firstPointIndex + j)(0), T(firstPointIndex + j)(1) - T(i)(1))
    }
    
    return emp
  }
}