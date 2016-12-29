package main

import scala.math._
// Point = [A,f,t]
// 1)On obtient de la constellation une liste T=[[A,f,t],[A,f,t],[A,f,t]...]
// 2)On trie T selon le temps
// 3)On trie T selon les fréquences si le temps est égal
// 4)On crée des targetzones de 6 points = [[1,2,3,4,5,6],[2,3,4,5,6,7],[3,4,5,6,7,8]]] avec 1=[A1,f1,t1] 2=[A2,f2,t2] etc
// 5)On crée les empreintes
object Fingerprinting {
  //Prends un tableau de 10 points en entree le tri selon le temps et retourne les empreintes
  def resume(T:Array[Array[Float]]):Array[Array[Float]]={
    triShell(T)
    var emp = empreinte(T)
    return emp
  }
 
 
     def triShell(a:Array[Array[Float]]):Unit={
    var gap = a.length/2
    while (gap >=1) {
      for (i <- gap until a.length){
        var j= i -gap
        var tmp0 = a(i)(0)
        var tmp1 = a(i)(1)
        var tmp2 = a(i)(2)
        while(j>=0 && tmp2 < a(j)(2)) {
          a(j+gap)(0)= a(j)(0)
          a(j+gap)(1) = a(j)(1)
          a(j+gap)(2)= a(j)(2)
          
          j-=gap
        
        }
        a(j+gap)(0) = tmp0
        a(j+gap)(1) = tmp1
        a(j+gap)(2) = tmp2
        
      }
      gap = (gap/2.2).round.toInt
    }
  }
   //On prend le 6eme point de la target zone et le point juste avant cette target zone pour faire une empreinte
   def empreinte(T:Array[Array[Float]]):Array[Array[Float]]={
     var emp:Array[Array[Float]]=Array.fill(T.length-6,3)(0)
     for (i <-0 to T.length - 7){
       emp(i) = Array(T(i)(1),T(i+6)(1),T(i+6)(2) - T(i)(2))
   }
    return emp
  }
 /* var a=Array.fill(15,3)(10*math.random.toFloat)
   println(a.deep.mkString("\n"))
   println("\n")
   
   var emp = resume(a)
println(emp.deep.mkString("\n"))*/
   
   
}

