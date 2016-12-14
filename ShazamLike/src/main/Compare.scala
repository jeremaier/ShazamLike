package main
import scala.math._
object Compare 
{

  
 
def empreinte(T:Array[Array[Float]]):(Array[Array[Float]])=
{
  //on suppose que le tableau est de la forme T=[[A,f,t],[ ], ...]
  //il faut que le tableau d'amplitudes soit trié en fonction du temps
  //on réorganise les amplitudes en fonction des fréquences (croissantes) (c'est ce qui sort de la fft)
  var TabBande=Array[Array[Float]]
  var Tabmax=Array.emptyFloatArray
  for (palier<-Array(10,20,40,80,160,511))
  {
    var compteur=0
    var TabBande=Array.emptyObjectArray
    while (math.log10(T(compteur)(1))<palier)
    {
      TabBande+:=T(i)
      compteur+=1
    }
    Tabmax+:=maxF(TabBande) //créer la fonction maxF d'un tableau
  }
  var moy=moyenne(Tabmax) //creer la fonction moyenne d'un tableau
  for (i<-0 to Tabmax.length-1)
  {
    if (Tabmax(i)(0)<moy) //on fait la moyenne des amplitudes
    {
      Tabmax-=Tabmax(i)
    }
  }
  return Tabmax
  }
  
  //on découpe l'étendue des fréquences en 6 bouts en échelle logarithme
  //on réalise une fonction max d'un tableau sur les amplitudes de chaque bande de fréquence
  //on calcule la moyenne des 6 valeurs obtenues
  //on garde seulement les points qui sont au dessus de la moyenne
  //on obtient notre empreinte
  
}

def maxF(T:Array[Float]):Float={
  var max : Float = 0
  for (i<- 0 to range T.length-1){
    if (T(i)>max) {
      max = T(i)
    }
  }
  return max
}
def moyenne(T:Array[Float]):Float={
  var s : Float= 0
  for (i<- 0 to T.length-1) {
    s = s + T(i)
  }
  return s/T.length.length
}



}