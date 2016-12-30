package main

import scala.math._

object Fingerprinting {
  //On obtient de la constellation une liste T = [[f,t],[f,t],[f,t]...]
  //On cree des targetzones de 5 points avec pour chacune un point d'ancrage (le 3eme avant chaque targetzone)
  def resume(T : Array[Array[Double]]) : Array[Array[Double]] = return empreinte(T)
  

  def empreinte(T : Array[Array[Double]]) : Array[Array[Double]] = {
    var emp : Array[Array[Double]] = Array.fill((T.length-7)*5,2)(0)
    var ancrage : Int = 0
    var ecart : Int = 3
    for(i <- 0 to (T.length-7)*5-1) {
      if (i%5 == 0 && i!=0){
        ancrage+=1
        ecart+= -4
        
      }
        emp(i) = Array(T(ancrage)(0), T(ecart)(0), T(ecart)(1) - T(ancrage)(1))
        ecart+=1
        
        
    }
    
    return emp
  }
 /*   var a=Array.fill(10,2)(10*math.random)
   println(a.deep.mkString("\n"))
   println("\n")
   
   var emp = resume(a)
println(emp.deep.mkString("\n"))*/
}