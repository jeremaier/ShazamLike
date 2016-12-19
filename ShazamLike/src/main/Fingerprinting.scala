package main

import scala.math._

object Fingerprinting {
  def empreinte(T : Array[Array[Float]]) : (Array[Array[Float]]) = {
    //on suppose que le tableau est de la forme T=[[A,f,t],[ ], ...]
    //il faut que le tableau d'amplitudes soit trie en fonction du temps
    //on reorganise les amplitudes en fonction des frequences (croissantes) (c'est ce qui sort de la fft)
    var Tabmax : Array[Array[Float]] = Array(Array())
    var bandes : Array[Int] = Array(0, 10, 20, 40, 80, 160, T.length - 1)
    for (i <- 0 to bandes.length - 2) { 
      var TabBande : Array[Array[Float]] = selection(T, bandes(i), bandes(i + 1)) //on selectionne les differentes bandes de frequence
      Tabmax +:= maxA(TabBande) //pour chaque bande de frequence on garde le max d'amplitude
      
      //var compteur = 0
      //while (math.log10(T(compteur)(1)) < palier)
      //{
        //TabBande +:= T(compteur)
        //compteur += 1
      //}
    }
    var moy = moyenneA(Tabmax)
    Tabmax = supMoyA(Tabmax, moy) //on garde seulement les amplitudes superieures a la moyenne
    return Tabmax
  }
    
  //on decoupe l'etendue des frequences en 6 bouts en echelle logarithme
  //on realise une fonction max d'un tableau sur les amplitudes de chaque bande de frequence
  //on calcule la moyenne des 6 valeurs obtenues
  //on garde seulement les points qui sont au dessus de la moyenne
  //on obtient notre empreinte(constellation plutot ?)
  
  // creation de la target zone : on trie les points obtenus de cette maniere : 
  // les points sont deja tries selon la frequence, on va d'abord les trier selon le temps par un tri a bulle
  //puis si deux points ont le meme temps, alors on trie selon la frequence
  //sinon on trie selon le temps
  
  def triTemps(T: Array[Array[Float]]) : (Array[Array[Float]]) = {
  for (i<-T.length -1 to 1 by -1){
    var tableauTrie:Boolean = true
    for (j<- 0 to i-1) {
    if (T(2)(j+1) < T(2)(j){
        T=echanger(j+1,j,T)// echanger est tout en bas
        tableauTrie = false
        if (tableauTrie) {
          return T
       }
    }
    }
  }
  return T
  }
// on obtient un tableau trie selon le temps 
// maintenant on parcourt ce tableau
// et on trie si les points ont la meme frequence
  def trifrequence(T:Array[Array[Float]]) : (Array[Array[Float]]) = {
    for (i<-0 to T.length-1) {
      if (T(2)(i) == T(2)(i+1) || T(1)(i) > T(1)(i+1)){ //si même temps mais la fq du pt d'avant est plus forte alors on echange
        T=echanger(i,i+1,T)
      }
    }
    return T // Ne marche pas si on a [A1,40,1] [A2,30,1] [A3,20,1] par exemple car sort [A2,30,1] [A3,20,1] [A1,40,1]
  //A modifier
  }
  
  def targetZone(T:Array[Array[Float]]) : (Array(Array(Array[Float]))) = {
    //Selon le site une target zone est composee de 6 points donc on cree un tableau de tableaux de 6 points(avec point =[A,f,t]
    var targetZones : Array(Array(Array[Float])) = Array(Array(Array()))
    for (i<0 to T.length -7) {
      targetZones(i) += T(i) + T(i+1) + T(i+2) + T(i+3) + T(i+4) + T(i+5)
    }
    return targetZones
  }
  //Le point d'ancrage est choisi comme étant le 3eme point avant le début de la target zone
    
  def selection(T : Array[Array[Float]], debut : Int, n : Int) : Array[Array[Float]] = {
    //fonction qui selectionne, a� partir d'un tableau T, n sous tableaux a partir de l'indice debut
    var selec : Array[Array[Float]] = Array(Array())
    for (i <- debut to n - debut)
      selec +:= T(i)
    
    return selec
  }

  def maxA(T:Array[Array[Float]]) : Array[Float] = {
    //fonction qui calcule l'element ayant l'amplitude la plus haute 
    //et qui renvoie cet element qui est un tableau
    //T est de la forme [[A,f,t],[A,f,t]]
    var TabMax:Array[Float] = Array()
    var max : Float = T(0)(0) 
    for (i<- 0 to T.length - 1) {
      if (T(i)(0) > max) {
        TabMax = T(i)
        max = T(i)(0)
      }
    }
    return TabMax
  }
  
  def moyenneA(T : Array[Array[Float]]) : Float = {
    //fonction qui calcule la moyenne de frequence des elements de T
    //T est de la forme [[A,f,t],[A,f,t]]
    var s : Float = 0
    for (i <- 0 to T.length - 1)
      s += T(i)(0)
    
    return s / T.length
  }
  
  def supMoyA(T : Array[Array[Float]], moy : Float) : Array[Array[Float]] = {
    //fonction qui garde les tableaux contenant des amplitudes superieures a moy
    var TabSup : Array[Array[Float]] = Array(Array())
    for (i <- 0 to T.length - 1) {
      if (T(i)(0) > moy)
        TabSup +:= T(i)
    }
    return TabSup
  }
  
  def echanger(indice1:Float,indice2:Float,T:Array[Array[Float]]) : (Array[Array[Float]]) = {
    var save = T(indice1)
    T(indice1)=T(indice2)
    T(indice2) = save
    return T
  }
}
