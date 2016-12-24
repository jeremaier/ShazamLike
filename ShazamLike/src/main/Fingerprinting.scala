package main

import scala.math._
// Point = [A,f,t]
// 1)On obtient de la constellation une liste T=[[A,f,t],[A,f,t],[A,f,t]...]
// 2)On trie T selon le temps
// 3)On trie T selon les fréquences si le temps est égal
// 4)On crée des targetzones de 6 points = [[1,2,3,4,5,6],[2,3,4,5,6,7],[3,4,5,6,7,8]]] avec 1=[A1,f1,t1] 2=[A2,f2,t2] etc
// 5)On crée les empreintes
object Fingerprinting {
  //fonction qui résume
  def resume(T:[Array[Array[Float]]):(Array[Array[Float])={
    var tab = triTemps(T)
    tab = triFrequence
    targetzones=targetZone(tab)
    fingerprint=empreinte(targetzones,tab)
    return targetzones,fingerprint
  }
    
    
    
    
    
    
  // creation de la target zone : on trie les points obtenus de cette maniere : 
  // les points sont deja tries selon la frequence, on va d'abord les trier selon le temps par un tri shell
  //puis si deux points ont le meme temps, alors on trie selon la frequence
  //sinon on trie selon le temps
   

  def triTemps(T: Array[Array[Float]]) : (Array[Array[Float]]) = {
    triShell(T,2) // le tri est fait plus bas
    return T
  }
// on obtient un tableau trie selon le temps 
// maintenant on parcourt ce tableau
// et on trie si les points ont la meme frequence

//on parcourt le tableau et on va mettre les points ayant meme t dans un autre tableau
  def triFrequence(T:Array[Array[Float]]) : (Array[Array[Float]]) = {
    var tab=T
    for (i<-0 to tab.length-1) {
      if (tab(2)(i) == tab(2)(i+1) || tab(1)(i) > tab(1)(i+1)){ //si même temps mais la fq du pt d'avant est plus forte alors on echange
        tab=echanger(i,i+1,tab)
      }
    }
    return T // Ne marche pas si on a [A1,40,1] [A2,30,1] [A3,20,1] par exemple car sort [A2,30,1] [A3,20,1] [A1,40,1]
  //A modifier
  }
  
  
    def targetZone(T:Array[Array[Float]]) : (Array[Array[Array[Float]]]) = {
      //Ex : [[1,2,3,4,5,6],[2,3,4,5,6,7],[3,4,5,6,7,8]]] avec 1=[A1,f1,t1] 2=[A2,f2,t2] etc
      //Selon le site une target zone est composee de 6 points donc on cree un tableau de tableaux de 6 points(avec point =[A,f,t]
    var targetZones : Array[Array[Array[Float]]] = Array(Array(Array()))
    for (i<-0 to T.length -7) {
      targetZones(i) ++ = [T(i),T(i+1),T(i+2),T(i+3),T(i+4),T(i+5)]
    }
    return targetZones
  }
   //Le point d'ancrage est choisi comme étant le 3eme point avant le début de la target zone selon le site, 
    //j'ai choisi de faire une empreinte avec chaque 2eme point de la taget zone
    
    def empreinte(targetZone:Array[Array[Array[Float]]],T:Array[Float])={
      var emp:Array[Array[Float]]=Array(Array())
      for (i<-3 to targetZone.length) {
        emp ++ = (T(i-3)(1),targetZone(i)(3)(1),targetZone(i)(3)(2)-T(i-3)(2)) // HELP JE SAIS PAS LECRIRE
        }
      //on retourne [fréquence du point d'ancrage, fréquence du point de début de target zone ,delta temps entre le point d'ancrage et le point] 
      return emp
    }
    
  def echanger(indice1:Int,indice2:Int,T:Array[Array[Float]]) : (Array[Array[Float]]) = {
    var save = T(indice1)
    T(indice1)=T(indice2)
    T(indice2) = save
    return T
  }
  //http://interactivepython.org/runestone/static/pythonds/SortSearch/TheShellSort.html
  def triShell(T:Array[Array[Float]],donnee:Int){
    var sublistcount = T.length//2
    while (sublistcount>0){
      for (startposition <- 0 to sublistcount){
        print("After increment of size", sublistcount, "The list is",alist)
        gapInstertionSort(T,startposition,sublistcount)
        sublistcount = sublistcount //2
      }
    }
    
  }
  
  def gapInsertionSort(T:Array[Array[Float]],donnee:Int,start:Int,gap:Int){
    var currentvalue = 0
    var position = 0
    for (i<-start+gap to T.length by gap){
      currentvalue = T(i)(donnee)
      position = i
      while (position >= gap || T(position-gap)(donnee)>currentvalue){
        T(position)(donnee)=T(position-gap)(donnee)
        position = position - gap
        T(position)(donnee)=currentvalue
      }
      T(position)(donnee)=currentvalue
        
      
    }
  }

}
