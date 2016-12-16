package main
import scala.math._
object Compare 
{

  
 
def empreinte(T:Array[Array[Float]]):(Array[Array[Float]])=
{
  //on suppose que le tableau est de la forme T=[[A,f,t],[ ], ...]
  //il faut que le tableau d'amplitudes soit trié en fonction du temps
  //on réorganise les amplitudes en fonction des fréquences (croissantes) (c'est ce qui sort de la fft)
  var TabBande:Array[Array[Float]]=Array(Array())
  var Tabmax:Array[Array[Float]]=Array(Array())
  for (palier<-Array(10,20,40,80,160,511))
  {
    //on crée les différentes bandes de fréquence
    //pour chaque bande de fréquence on ajoute les élements au tableau TabBande et on calcule le max et la moyenne
    //on calcule la moyenne de chaque bande de fréquence
    var compteur=0
    var TabBande:Array[Array[Float]]=Array(Array())
    while (math.log10(T(compteur)(1))<palier)
    {
      TabBande+:=T(compteur)
      compteur+=1
    }
    Tabmax+:=maxA(TabBande) //pour chaque bande de fréquence on garde le max d'amplitude
  }
  var moy=moyenneA(Tabmax)
  Tabmax=supMoyA(Tabmax,moy)//on garde seulement les amplitudes supérieures à la moyenne
  return Tabmax
  }
  
  //on découpe l'étendue des fréquences en 6 bouts en échelle logarithme
  //on réalise une fonction max d'un tableau sur les amplitudes de chaque bande de fréquence
  //on calcule la moyenne des 6 valeurs obtenues
  //on garde seulement les points qui sont au dessus de la moyenne
  //on obtient notre empreinte
  

def maxA(T:Array[Array[Float]]):Array[Float]={
  //fonction qui calcule l'élement ayant l'amplitude la plus haute 
  //et qui renvoie cet élement qui est un tableau
  //T est de la forme [[A,f,t],[A,f,t]]
  var TabMax:Array[Float]=Array()
  var max : Float = T(0)(0) 
  for (i<- 0 to T.length-1){
    if (T(i)(0)>max) {
      TabMax = T(i)
      max=T(i)(0)
    }
  }
  return TabMax
}
def moyenneA(T:Array[Array[Float]]):Float={
  //fonction qui calcule l'élement ayant la fréquence la plus haute 
  //T est de la forme [[A,f,t],[A,f,t]]
  var s : Float= 0
  for (i<- 0 to T.length-1) {
    s = s+T(i)(0)
  }
  return s/T.length
}

def supMoyA(T:Array[Array[Float]],moy:Float):Array[Array[Float]]={
  //fonction qui garde les tableaux contenant des amplitudes supérieures à moy
  var TabSup:Array[Array[Float]]=Array(Array())
  for (i<- 0 to T.length-1)
  {
    if (T(i)(0)>moy)
    {
      TabSup+:=T(i)
    }
  }
  return TabSup
}




}