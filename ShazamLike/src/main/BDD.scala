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
	var timeAnalysis : Long = 0
		      
  //Creation des fichiers et dossiers necessaires a l'analyse de BDD s'il n'existe pas
  def CreateFoldersAndCache() {
	  if(!folder.exists())
	    folder.mkdirs()
	  cacheFile.createNewFile()
	  dateFile.createNewFile()
	}
  
	//Verifie si le dossier BDD a ete modifie ou pas depuis le dernier demarrage
	def IsModif() {
	  val dateBDD : String = folder.lastModified().toString()
  	val cacheBr : BufferedReader = new BufferedReader(new FileReader(dateFile))
	  
	  if(dateBDD != cacheBr.readLine())
	    modif = true
	  
	  cacheBr.close()
	  DateCacheWrite(dateBDD)
	}
	
	//Actualisation de la date du dossier dans le fichier date.txt
	def DateCacheWrite(dateBDD : String) {
	  val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(dateFile, false))
	  cacheBw.write(dateBDD)
	  cacheBw.close()
	}
	
	//Ecrit le resultat des analyses de BDD dans le cache.txt
	def CacheWriter(songNames : Array[String], fingerPrints : Array[Array[(Long, Int)]]) {
  	val cachePw : PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(cacheFile, false)))
  	
  	for(i <- 0 to songNames.length - 1) {
    	cachePw.print(songNames(i) + "\t" + fingerPrints(i).length + "\t")
    	
      for(j <- 0 to fingerPrints(i).length - 1) {
        var (hash, time) = fingerPrints(i)(j)
        cachePw.print(hash + "\t" + time + "\t")
      }
    	
    	cachePw.print("\n")
  	}
  	
	  cachePw.close()
	}
	
	//Lecture des noms des fichiers dans cache.txt qui sont presents dans la BDD
	def CacheReaderName(fileNumber : Int) : Array[String] = {
  	val cacheS : Scanner = new Scanner(cacheFile)
  	var nameFile : Array[String] = new Array[String](directoryFilesName.length)
  	
    for(i <- 0 to fileNumber - 1) {
      nameFile(i) = cacheS.next()
      cacheS.nextLine()
    }
	  
	  cacheS.close()
	  
	  return nameFile
	}
	
	//Lecture de l'ensemble des empreintes de chaque musique presente dans la BDD
	def CacheReaderFingerPrint(fileNumber : Int) : Array[Array[(Long, Int)]] = {
	  val empreinte : Array[Array[(Long, Int)]] = new Array(fileNumber)
  	val cacheS : Scanner = new Scanner(cacheFile)
    
    for(i <- 0 to fileNumber - 1) {
      cacheS.next()
      var fingerPrintLength : Int = cacheS.next().toInt
      empreinte(i) = new Array(fingerPrintLength)
      
      for(j <- 0 to empreinte(i).length - 1)
  	    empreinte(i)(j) = (cacheS.next().toLong, cacheS.next().toInt)
      
      cacheS.nextLine()
    }
	  
	  cacheS.close()
	  
	  return empreinte
	}
  
	//Ensemble des verifications avant le lancement soit de la creation d'empreintes soit de la lecture dans les fichiers de ces empreintes
	def DirectoryAnalysisLaunch() {
	  timeAnalysis = System.currentTimeMillis()
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
	    println("Analyse du cache terminée en : " + (System.currentTimeMillis() - timeAnalysis) / 1000F)
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