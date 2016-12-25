package main
import scala.math._
object Compare 
{
//objectif: comparer un échantillon avec chaque morceau de la base de donnee
//1: pour un echantillon, on calcule le taux le compatibilité avec chaque morceau de la base de donnee
//2: on garde que ceux dont le taux est superieur à 90% et on garde les targetZones concernees
//3: on vérifie la cohérence temporelle entre chaque targetZone de l'enchantillon et celle du morceau
//4: on garde celles qui sont cohérentes
//5: 

def matching(sample:Array[Array[Float]],database:Array[Array{Float]]):Array={
  //on prendra database=Fingerprinting.resume(
  
}

def compareSong(sample:Array[Array[Float]],database:Array[Array[Float]]):Float={
  //fonction qui prend en entrée un echantillon et un morceau
  //et qui renvoie le pourcentage de "matching"
  //seulement si la cohérence temporelle est réalisée
  for (i<-0 to database.length-1){
    
    }
}
  
def coherenceTempZone(T : Array[Array[Array[Float]]]):Boolean={
  //fonction qui prend en entree deux listes: 
  //sampleZone est une target Zone de l'enchantillon
  //songZones est une liste de target zones correspondantes dans un morceau
  //et qui renvoie 
  //T=[[[sampleZone1],[songZoneassociee1a],[songZoneassociee1b],...],[[sampleZone2],[[songZoneassociee2a],[songZoneassociee2b]]]]
  var deltaTab:Array[Float]=Array()
  var zone:Array[Array[Float]]=Array(Array())
  for (i<-0 to T.length-1){
    var zone=T(i) //cette variable contient[[sampleZone1],[songZoneassociee1a],[songZoneassociee1b],...]
    for (j<-0 to zone.length-2){
      deltaTab(i+j)=math.abs(zone(0)(2)-zone(j)(2))
      //on ajoute à deltaTab tous les delta de temps
      }
    }
  if (countDelta(maxDelta(deltaTab)){}//c'est le nombre de fois que le delta ayant le plus d'occurences apparait
    
  
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
  
  
def matchingRate(sample : Array[Array[Float]], song : Array[Array[Float]]):Float={
  //fonction qui compare un echantillon et une chanson entière
  //et retourne leur taux de "matching"
  //la forme des données song et sample sont des tableaux de la forme
  // [[f-ancrage,f-point,delta-temps],[f-ancrage,f-point,delta-temps]]
  //ce sont donc deux tableaux contenant des target zones
  var rate:Float=0
  for (i<-0 to sample.length-1){
    for (j<-0 to song.length-1)
      {
      if (sample(i)(0)==song(j)(0) && sample(i)(1)==song(j)(1)){
        //ici la comparaison n'est faite que sur les fréquences
        rate=rate+1 //une targetZone ne peut être présente qu'une seule fois dans chaque liste
        }
      }
    }
  return rate/sample.length
  }
}