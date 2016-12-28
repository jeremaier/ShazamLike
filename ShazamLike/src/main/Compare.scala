package main
import scala.math._
object Compare 
{
//objectif: comparer un échantillon avec chaque morceau de la base de donnee
//1: pour un echantillon, on calcule le taux le compatibilité avec chaque morceau de la base de donnee
//2: on garde que ceux dont le taux est superieur à 90% et on garde les empreintes concernees
//3: on vérifie la cohérence temporelle entre chaque empreinte de l'enchantillon et celle du morceau
//4: on garde celle qui est la plus cohérente temporellement
  // i.e. celle qui a le plus grand nombre de delta cohérent

def matching(sample:Array[Array[Float]],database:Array[Array[Array[Float]]]):Float={
  //fonction qui prend en entrée un échantillon sous forme d'empreinte
  //et une base de donnée sous la forme [[[ID1],[empreinte1],[empreinte2]],[[ID2],[empreinte1],[empreinte2]]]
  //et qui renvoie l'identifiant de la chanson la plus susceptible de matcher avec l'echantillon
  var empreintes=database
  var potentialMatching:Array[Array[Array[Float]]]=Array(Array(Array()))
  var j=0 //indice d'incrémentation de la liste potentialMatching
  for (i<-0 to empreintes.length-1){
    var song=empreintes(i).slice(1,empreintes.length)//on retire l'id de la chanson
    if (matchingRate(sample,song)._1>=0.8){
      potentialMatching(j)=empreintes(i)
      j+=1
      }
    //si on a un taux de matching supérieur à 80%
    //on ajoute la chanson à une liste 
    //puis on fait l'analyse temporelle:
    }
  var nbMaxDelta:Array[Float]=Array()
  for (k<-0 to potentialMatching.length-1){
    //pour chaque morceau potentiellement ressemblant
    var potentialSong=potentialMatching(k).slice(1,potentialMatching.length)
    var correspondingTab=matchingRate(sample,potentialSong)._2
    //tableau des correspondances de chaque empreinte de sample avec celles de song
    var deltaTab:Array[Float]=Array()
    for (l<-0 to correspondingTab.length-1){
      deltaTab++computeDelta(correspondingTab(l))
      //on ajoute a la liste des delta les delta pour chaque correspondance de sample
      }
    nbMaxDelta(k)=countDelta(deltaTab,maxDelta(deltaTab))
    //on rajoute le nombre d'occurences du delta le plus présent dans deltaTab
    //pour chaque chanson
    }
  return potentialMatching(indiceMax(nbMaxDelta))(0)(0)
  //on retourne l'ID de la chanson matchant à plus de 80% et cohérente temporellement
}

def matchingRate(sample : Array[Array[Float]], song : Array[Array[Float]]):(Float,Array[Array[Array[Float]]])={
  //fonction qui compare un echantillon et une chanson entière
  //et retourne leur taux de "matching" 
  //ainsi que la liste des empreintes qui correspondent entre elles
  //sous la forme [[[sampleEmpreinte1],[correspondingEmpreinte1A],[correspondingEmpreinte1B]],[[sampleEmpreinte1],[correspondingEmpreinte1A],[correspondingEmpreinte1B]]]
  //la forme des données song et sample sont des tableaux de la forme
  // [[f-ancrage,f-point,delta-temps],[f-ancrage,f-point,delta-temps]]
  //ce sont donc deux tableaux contenant des empreintes
  var rate:Float=0
  var correspondingFingerprints:Array[Array[Array[Float]]]=Array(Array(Array()))
  //tableau contenant la liste des empreintes de sample avec leurs correspondances dans song
  var k=0 //indice pour remplir correspondingFingerprints avec chaque liste de correpondance
  var l=1 //indice pour remplir correspondingFingerprints avec chaque correspondance
  for (i<-0 to sample.length-1){
    var rempli:Boolean=false//pour savoir si le sample d'indice i a une correspondance
    correspondingFingerprints(k)(0)=sample(i) //on rajoute l'élément de sample
    for (j<-0 to song.length-1){
      if (sample(i)(0)==song(j)(0) && sample(i)(1)==song(j)(1)){
        //on compare les deux fréquences présentes dans les empreintes
        rate=rate+1 
        //rq: on suppose qu'une empreinte ne peut être présente qu'une seule fois dans chaque liste  
        correspondingFingerprints(k)(l)=song(j)
        l+=1
        rempli=true
        }
      }
    if (rempli){
      k+=1
      } 
      //si on a eu des correspondances avec sample(i)
      //on rempli correspondingFingerprints a la suite
      //sinon on reprend cet indice k non rempli 
      //et on l'utilise pour stocker les new valeurs de l'indice suivant
    }
  var taux:Float=rate/sample.length
  return (taux,correspondingFingerprints)
  }
  
def computeDelta(T : Array[Array[Float]]):Array[Float]={
  //fonction qui prend en entree une liste sous la forme
  //[[sample],[correspondance1],[correspondance2]]
  //et qui renvoie une liste de delta calculés chacuns pour une correspondance
  var deltaTab:Array[Float]=Array()
  for (i<-1 to T.length-1){
      deltaTab(i-1)=math.abs(T(0)(2)-T(i)(2))
      //on ajoute à deltaTab tous les delta de temps
    }
  return deltaTab
  }  
  
def maxDelta(deltaTab:Array[Float]):Float={
//fonction qui prend en argument une liste de delta 
//et qui retourne la valeur ayant le plus d'occurences dans la liste
  var deltamax=deltaTab(0)
  for (i<-0 to deltaTab.length-2){
    if (countDelta(deltaTab,deltaTab(i+1))>countDelta(deltaTab,deltaTab(i))){
      //on compare le nombre de fois qu'existe un delta dans le tableau
      deltamax=deltaTab(i+1)
      }
    }
  return deltamax
  }

def countDelta(deltaTab:Array[Float],delta:Float):Int={  
  //fonction qui prend en entrée une liste de deltas et une valeur de delta
  //et qui ressort le nombre d'occurences de cette valeur
  var countDelta:Int=0
  var deltamax=deltaTab(0)
  for (i<-1 to deltaTab.length-1){
    if (deltaTab(i)==delta){
      countDelta+=1
      }
    }
  return countDelta
  }
  
def indiceMax(T:Array[Float]):Int={
  //fonction qui prend en entree une liste T
  //et qui renvoie l'indice du maximum de la liste
  var indice=0
  for (i<-1 to T.length-1){
    if (T(i)>T(indice)){
      indice=i
      }
    }
  return indice
  }
}