package main

import scala.math._

object Fingerprinting {
  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage (le 3eme avant chaque targetzone)  
  def FingerPrint(T : Array[Array[Double]]) : Array[Double] = {
    val emp : Array[Double] = new Array[Double]((T.length - 7) * 5 * 3)
    var ancrage : Int = 0
    var ecart : Int = 3
    
    for(i <- 0 to emp.length - 1 by 3) {
      if(i % 5 == 0 && i != 0) {
        ancrage += 1
        ecart -= 4
      }
      
      emp(i) = T(ancrage)(0)
      emp(i + 1) = T(ecart)(0)
      emp(i + 2) = T(ecart)(1) - T(ancrage)(1)

      ecart += 1
    }
    return emp
  }
  
  /*var a=Array.fill(10,2)(10*math.random)
  println(a.deep.mkString("\n"))
  println("\n")
  var emp = resume(a)
	println(emp.deep.mkString("\n"))*/
}