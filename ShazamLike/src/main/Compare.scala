package main

object Compare 
{
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
      if (sample(i)(0)==song(j)(0) && sample(i)(1)==song(j)(1) && sample(i)(2)==song(j)(2)){
        rate=rate+1 //une targetZone ne peut être présente qu'une seule fois dans chaque liste
        }
      }
    }
  return rate/sample.length
  }


}