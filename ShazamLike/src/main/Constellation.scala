

package main

object Constellation {
  def empreinte(T : Array[Array[Float]]) : Array[Array[Float]] = {
    //on suppose que le tableau est de la forme T=[[A,f,t],[ ], ...]
    //il faut que le tableau d'amplitudes soit trie en fonction du temps
    //on reorganise les amplitudes en fonction des frequences (croissantes) (c'est ce qui sort de la fft)
    var Tabmax : Array[Array[Float]] = Array(Array())
    var j=0
    var bandes = arrayGen2x(0,Tabmax.length)
    for (i <- 0 to bandes.length - 2) { 
      var TabBande = select(T, bandes(i), bandes(i + 1)) //on selectionne les differentes bandes de frequence
      if (T(i)(1)<=5000){ //on prend que les frequences inférieures à 5000Hz
        Tabmax(j)= maxA(TabBande) //pour chaque bande de frequence on garde le max d'amplitude
        j+=1
        }
      }
    Tabmax = supMoyA(Tabmax, moyenneA(Tabmax)) //on garde seulement les amplitudes superieures a la moyenne
    return Tabmax
  }
  
  
 def arrayGen2x(debut:Int,fin:Int):Array[Float]={
    //fonction permettant de générer un tableau de 2*x
    var liste:Array[Float]=Array(debut)
    var i=0
    while(i<fin){
      if (liste(i)==0){
        liste(i+1)=2
        }
      else{
        liste(i+1)=2*liste(i)
        }
      i+=1
      return liste
    }
    //on decoupe l'etendue des frequences en 6 bouts en echelle logarithme
  //on realise une fonction max d'un tableau sur les amplitudes de chaque bande de frequence
  //on calcule la moyenne des 6 valeurs obtenues
  //on garde seulement les points qui sont au dessus de la moyenne
  //on obtient notre empreinte
  
  
  //fonctions auxiliaires necessaires
  
  def select(T : Array[Array[Float]], debut : Int, fin : Int) : Array[Array[Float]] = {
    //fonction qui selectionne, a partir d'un tableau T
    //un sous tableau qui commence à debut et termine à fin 
    var selec : Array[Array[Float]] = Array(Array())
    for (i <- debut to fin){
      selec(i-debut) = T(i)
      }
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
}