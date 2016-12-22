package main

import scala.math._

object Fingerprinting {
  // creation de la target zone : on trie les points obtenus de cette maniere : 
  // les points sont deja tries selon la frequence, on va d'abord les trier selon le temps par un tri a bulle
  //puis si deux points ont le meme temps, alors on trie selon la frequence
  //sinon on trie selon le temps
   

  def triTemps(T: Array[Array[Float]]) : (Array[Array[Float]]) = {
    triBulle(T,2) // le tri est fait plus bas
    return T
  }
// on obtient un tableau trie selon le temps 
// maintenant on parcourt ce tableau
// et on trie si les points ont la meme frequence

//on parcourt le tableau et on va mettre les points ayant meme t dans un autre tableau
  def trifrequence(T:Array[Array[Float]]) : (Array[Array[Float]]) = {
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
    //Selon le site une target zone est composee de 6 points donc on cree un tableau de tableaux de 6 points(avec point =[A,f,t]
    var targetZones : Array[Array[Array[Float]]] = Array(Array(Array()))
    for (i<-0 to T.length -7) {
      targetZones(i) = T(i) + T(i+1) + T(i+2) + T(i+3) + T(i+4) + T(i+5)
    }
    return targetZones
  }
   //Le point d'ancrage est choisi comme étant le 3eme point avant le début de la target zone selon le site, 
    //j'ai choisi de faire une empreinte avec chaque 2eme point de la taget zone
    
    def empreinte(targetZone:Array[Array[Array[Float]]],T:Array[Float])={
      var emp:Array[Array[Float]]=Array(Array())
      for (i<-3 to targetZone.length) {
        emp += (T(i-3)(1),targetZone(i)(3)(1),targetZone(i)(3)(2)-T(i-3)(2)) // HELP JE SAIS PAS LECRIRE
        }
      return emp
    }
    
  def echanger(indice1:Int,indice2:Int,T:Array[Array[Float]]) : (Array[Array[Float]]) = {
    var save = T(indice1)
    T(indice1)=T(indice2)
    T(indice2) = save
    return T
  }
  
  def triBulle(T:Array[Array[Float]],donnee:Int){
    var tab=T
    for (i<-tab.length -1 to 1 by -1){
    var tableauTrie:Boolean = true
    for (j<- 0 to i-1) {
    if (tab(donnee)(j+1) < tab(donnee)(j)){
        tab=echanger(j+1,j,tab) // echanger est en haut (a modifier ?)
        tableauTrie = false
        if (tableauTrie) {
          return false //on arrete la boucle
       }
    }
  }
  

}
