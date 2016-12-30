package main

object Constellation {
  //Tableau de la forme[A, A, A, A, A,...]
  //Les frequences sont k * filesParameters(0)(0).toFloat / sampleLength
  //Le temps de ces frequences sont la taille des fenetres selectionnes * 0.1 seconde
  def empreinte(frequencyAmplitudes : Array[Double], sampleLength : Int, samplingFrequency : Int) : Array[Array[Double]] = {
    val maxFrequencies : Array[Array[Double]] = Max(frequencyAmplitudes, bandeGen(sampleLength), samplingFrequency)
    
    return SupMean(maxFrequencies, Mean(frequencyAmplitudes))
  }
  
  //Generation des limites de bandes de frequences
  def bandeGen(limit : Int) : Array[Int] = {
    var bandes : Array[Int] = new Array(7)
    var sep : Int = 10 * limit / 1024
    
    bandes(0) = 0

    for(i <- 1 to 5) {
      bandes(i) = sep
      sep *= 2
    }
    
    bandes(6) = limit / 2 - 1
    
    return bandes
  }

  //Renvoi le tableau des amplitudes les plus hautes avec la frequence correspondante
  def Max(frequencyAmplitudes : Array[Double], bandes : Array[Int], samplingFrequency : Int) : Array[Array[Double]] = {
    val sampleLength = (bandes(6) + 1) * 2
    val samples = frequencyAmplitudes.length / sampleLength
    val tabMax : Array[Array[Double]] = new Array(6 * samples)
    
      for(i <- 0 to samples - 1) {
        var firstIndex : Int = i * sampleLength
        var max : Double = frequencyAmplitudes(firstIndex)
        var index : Int = 0
                
        for(j <- 0 to bandes.length - 2) {
          for(k <- bandes(j) to bandes(j + 1)) {
            var l = firstIndex + k
            
            if (frequencyAmplitudes(l) > max) {
              max = frequencyAmplitudes(l)
              index = l - sampleLength * i
            }
          }
          tabMax(i) = Array[Double](max, index * samplingFrequency.toFloat / sampleLength)
        }
      }
    
    return tabMax
  }
  
  //Renvoi la moyenne des amplitudes frequentielles
  def Mean(frequencyAmplitudes : Array[Double]) : Double = {
    var sum : Double = 0
    val length : Int = frequencyAmplitudes.length
    
    for (i <- 0 to length - 1)
      sum += frequencyAmplitudes(i)
    
    return sum.toFloat / length
  }
  
  //Retourne le tableau des amplitudes max avec la frequence et le temps qui correspond
  def SupMean(maxArray : Array[Array[Double]], mean : Double) : Array[Array[Double]] = {
    var supArray : Array[Array[Double]] = Array()
    
    for(i <- 0 to maxArray.length - 1) {
      if(maxArray(i)(0) >= mean)
        supArray +:= Array[Double](maxArray(i)(1), i / 6 * 0.1)
    }
    
    return supArray
  }
}