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
	var modif : Boolean = false
  var errorMessage : String = "Attention, le dossier par defaut est vide ou ne contient pas que des fichiers .wav"
	      
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
	  dateCacheWrite(dateBDD)
	}
	
	//Actualisation de la date du dossier
	def dateCacheWrite(dateBDD : String) {
	  val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(dateFile, false))
	  cacheBw.write(dateBDD)
	  cacheBw.close()
	}
	
	//Ecrit le resultat des analyses de BDD precedentes
	def CacheWriter(songNames : Array[String], fingerPrints : Array[Array[Double]]) {
  	val cachePw : PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(cacheFile, false)))
  	
  	for(i <- 0 to songNames.length - 1) {
    	cachePw.print(songNames(i) + "\t")
    	
      for(j <- 0 to fingerPrints(i).length - 1 by 3) {
    	  for(k <- 0 to 2)
    	    cachePw.print(fingerPrints(i)(j + k) + "\t")
    	}
    	
    	cachePw.print("-\n")
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
	def CacheReaderFingerPrint(fileNumber : Int) : Array[Array[Double]] = {
	  val empreinte : Array[Array[Double]] = new Array[Array[Double]](fileNumber)
  	val cacheS : Scanner = new Scanner(cacheFile)
    object allDone extends Exception {}
    
    try {
      for(i <- 0 to fileNumber - 1) {
        var j = 0
        cacheS.next()
        
        while(cacheS.hasNext() && !cacheS.hasNext(".")) {
    	    empreinte(i)(j) = cacheS.next().toDouble
      	  j += 1
      	}
        
        cacheS.nextLine()
      }
    } catch {case allDone : Throwable =>}
	  
	  cacheS.close()
	  
	  return empreinte
	}
  
	//Ensemble des verifications avant le lancement de l'analyse de la BDD
	def DirectoryAnalysisLaunch(peer : java.awt.Component) {
	  IsModif()
	  
	  if(modif) {
  	  if(IsDirectoryFilesAndWav(peer))
  	    DirectoryFilesAnalysis(directoryFilesName, directoryPath)
  	  
  	  modif = false
  	  errorMessageWindow(peer, "Le dossier base de donnée a été modifié ou n'existait pas")
	  } else {
	    var fileNumber : Int = directoryFilesName.length
      filesNames = CacheReaderName(fileNumber)
      fingerPrintsDirectory = CacheReaderFingerPrint(fileNumber)
	    SetReadyBDD(true)
	  }
  }
	
	//Verifie que le dossier BDD ne contient que des fichiers wav
	def IsDirectoryFilesAndWav(peer : java.awt.Component) : Boolean = {
	  if(IsDirectoryFiles()) {
      for(i <- 0 to directoryFilesName.length - 1) {
        if(!directoryFilesName(i).toString().endsWith(".wav")) {
          errorMessage = "Le dossier ne contient pas que des fichiers .wav"
          return false
        }
	    }
	    return true
	  } else {
      errorMessage = "Le dossier des sons ne contient aucun fichier"
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