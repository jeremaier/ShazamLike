package main

import java.io._
import java.util._
import GUI._
import Import._
import scala.collection.mutable._

object BDD {
  var directoryPath : String = System.getProperty("user.dir") + "\\BDD"
  var folder : File = new File(directoryPath)
	var cacheFile : File = new File(directoryPath + "\\cache.txt")
  var dateFile : File = new File(directoryPath + "\\date.txt")
	var modif : Boolean = true
	      
  //Creation des fichiers et dossiers necessaires a l'analyse de BDD s'il n'existe pas
  def CreateFoldersAndCache() {
	  if(!folder.exists())
	    folder.mkdirs()
	  cacheFile.createNewFile()
	  dateFile.createNewFile()
	}
  
	//Verifie si le dossier BDD a ete modifie ou pas pour ne pas faire l'analyse 2 fois
	def IsModif() {
	  val dateBDD : String = folder.lastModified().toString()
  	val cacheBr : BufferedReader = new BufferedReader(new FileReader(dateFile))
	  
	  if(dateBDD != cacheBr.readLine())
	    modif = true
	  
	  cacheBr.close()
	  DateCacheWrite(dateBDD)
	}
	
	//Actualisation de la date du dossier
	def DateCacheWrite(dateBDD : String) {
	  val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(dateFile, false))
	  cacheBw.write(dateBDD)
	  cacheBw.close()
	}
	
	//Ecrit le resultat des analyses de BDD precedentes
	def CacheWriter(songNames : Array[String], fingerPrints : Array[Array[Array[Double]]]) {
  	val cachePw : PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(cacheFile, false)))
  	
  	for(i <- 0 to songNames.length - 1) {
    	cachePw.print(songNames(i) + "\t" + fingerPrints(i).length + "\t")
    	
      for(j <- 0 to fingerPrints(i).length - 1) {
    	  for(k <- 0 to 2)
    	    cachePw.print(fingerPrints(i)(j)(k) + "\t")
    	}
    	
    	cachePw.print("\n")
  	}
  	
	  cachePw.close()
	}
	
	//Lecture du nom du fichier qui correspond a la ligne
	def CacheReaderName(fileNumber : Int) : Array[String] = {
  	val cacheS : Scanner = new Scanner(cacheFile)
  	var nameFile : Array[String] = new Array[String](directoryFilesName.length)
    object allDone extends Exception {}
    
    try {
      for(i <- 0 to fileNumber - 1) {
        nameFile(i) = cacheS.next()
        cacheS.nextLine()
      }
    } catch {case allDone : Throwable =>}
	  
	  cacheS.close()
	  
	  return nameFile
	}
	
	//Lecture de l'ensemble des nombres sur une seule ligne pour reconstituer le tableau d'empreintes d'un fichier
	def CacheReaderFingerPrint(fileNumber : Int) : Array[Array[Array[Double]]] = {
	  val empreinte : Array[Array[Array[Double]]] = new Array(fileNumber)
  	val cacheS : Scanner = new Scanner(cacheFile)
    object allDone extends Exception {}
    
    try {
      for(i <- 0 to fileNumber - 1) {
        cacheS.next()
        var fingerPrintLength : Int = cacheS.next().toInt
        var empreintePerFile : Array[Array[Double]] = new Array(fingerPrintLength)
        
        for(j <- 0 to empreintePerFile.length - 1)
          for(k <- 0 to 2)
    	      empreintePerFile(j)(k) = cacheS.next().toDouble
    	  
        cacheS.nextLine()
        empreinte(i) = empreintePerFile
      }
    } catch {case allDone : Throwable =>}
	  
	  cacheS.close()
	  
	  return empreinte
	}
  
	//Ensemble des verifications avant le lancement de l'analyse de la BDD
	def DirectoryAnalysisLaunch() {
	  IsModif()
	  
	  if(modif) {
  	  if(IsDirectoryFilesAndWav())
  	    DirectoryFilesAnalysis(directoryFilesName, directoryPath)
  	  
  	  errorOrInfoWindow("Le dossier base de donnée a été modifié ou n'existait pas", "Erreur")
  	  modif = false
	  } else {
	    var fileNumber : Int = directoryFilesName.length
      filesNames = CacheReaderName(fileNumber)
      fingerPrintsDirectory = CacheReaderFingerPrint(fileNumber)
	    SetReadyBDD(true)
	  }
  }
	
	//Verifie que le dossier BDD ne contient que des fichiers wav
	def IsDirectoryFilesAndWav() : Boolean = {
	  if(IsDirectoryFiles()) {
      for(i <- 0 to directoryFilesName.length - 1) {
        if(!directoryFilesName(i).toString().endsWith(".wav")) {
          errorOrInfoWindow("Le dossier ne contient pas que des fichiers .wav", "Erreur")
          return false
        }
	    }
	    return true
	  } else {
	    errorOrInfoWindow("Le dossier des sons ne contient aucun fichier", "Erreur")
	    return false
	  }
	}
	
	//Verifie s'il y a des fichiers dans le dossier BDD
	def IsDirectoryFiles() : Boolean = {
	  if(directoryFilesName.length > 0)
	    return true
	  else return false
	}
	
	//Changement de dossier d'analyse
	def RefreshFoldersAndCacheDirectories() {
	  folder = new File(directoryPath)
	  cacheFile = new File(directoryPath + "\\cache.txt")
	  dateFile = new File(directoryPath + "\\date.txt")
	}
}