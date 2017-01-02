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
	
	def dateCacheWrite(dateBDD : String) {
	  val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(dateFile))
	  cacheBw.append(dateBDD)
	  cacheBw.close()
	}
	
	//Lecture du nom du fichier qui correspond a la ligne
	def CacheReaderName(cacheFileNumber : Int, line : Int) : String = {
  	val cacheS : Scanner = new Scanner(cacheFile)
  	var nameFile : String = ""
    object allDone extends Exception {}
      
    try {
      for(i <- 0 to line - 1)
        cacheS.nextLine()
      
      nameFile = cacheS.next()
    } catch {case allDone : Throwable =>}
	  
	  cacheS.close()

	  return nameFile
	}
	
	//Lecture de l'ensemble des nombres sur une seule ligne pour reconstituer le tableau d'empreintes d'un fichier
	def CacheReaderFingerPrint(cacheFileNumber : Int, line : Int) : Array[Double] = {
	  val empreinte : ArrayBuffer[Double] = ArrayBuffer[Double]()
  	val cacheS : Scanner = new Scanner(cacheFile)
	  
    object allDone extends Exception {}
    
    try {
      for(i <- 0 to line - 1)
        cacheS.nextLine()

      var i = 0

      cacheS.next()
      while(cacheS.hasNext() && !cacheS.hasNext(".")) {
  	    empreinte += cacheS.next().toDouble
    	  i += 1
    	}
    } catch {case allDone : Throwable =>}
	  
	  cacheS.close()
	  
	  return empreinte.toArray
	}
	
	//Ecrit le resultat des analyses de BDD precedentes
	def CacheWriter(cacheFileNumber : Int, songNames : Array[String], fingerPrints : Array[Array[Double]]) {
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
  
	//Ensemble des verifications avant le lancement de l'analyse de la BDD
	def DirectoryAnalysisLaunch(peer : java.awt.Component) {
	  IsModif()
	  
	  if(modif) {
  	  if(IsDirectoryFilesAndWav(peer))
  	    DirectoryFilesAnalysis(directoryFilesNames, directoryPath)
  	  
  	  modif = false
  	  errorMessageWindow(peer, "Le dossier base de donnée a été modifié depuis la dernière utilisation")
	  } else {
	    val filesNumbers : Array[Int] = new Array[Int](3)
	    
	    for(i <- 0 to 2)
	      filesNumbers(i) = directoryFilesName(i).length
	    
	    fingerPrintsDirectory = new Array[Array[Array[Double]]](3)
	    filesNames = new Array[Array[String]](3)
	    
	    for(i <- 0 to 2) {
	      filesNames(i) = new Array[String](filesNumbers(i))
	      fingerPrintsDirectory(i) = Array.ofDim[Double](filesNumbers(i), 0)
	      
	      for(j <- 0 to filesNumbers(i) - 1) {
	        filesNames(i)(j) = CacheReaderName(i + 1, j)
	        fingerPrintsDirectory(i)(j) = CacheReaderFingerPrint(i + 1, j)
	      }
	    }
	    
	    SetReadyBDD(true)
	  }
  }
	
	//Verifie que le dossier BDD ne contient que des fichiers wav
	def IsDirectoryFilesAndWav(folderNumber : Int, peer : java.awt.Component) : Boolean = {
	  val files : Array[String] = directoryFilesName(folderNumber)
	  
	  if(IsDirectoryFiles()) {
      for(i <- 0 to files.length - 1) {
        if(!files(i).toString().endsWith(".wav")) {
          folderNumber match {
            case 1 => errorMessage = "Le dossier des sons de 22kHz ne contient pas que des fichiers .wav"
            case 2 => errorMessage = "Le dossier des sons de 44kHz ne contient pas que des fichiers .wav"
            case _ => errorMessage = "Le dossier des sons de 11kHz ne contient pas que des fichiers .wav"
          }
          return false
        }
	    }
	    return true
	  } else {
	    folderNumber match {
        case 1 => errorMessage = "Le dossier des sons de 11kHz ne contient aucun fichier"
        case 2 => errorMessage = "Le dossier des sons de 11kHz ne contient aucun fichier"
        case _ => errorMessage = "Le dossier des sons de 11kHz ne contient aucun fichier"
      }
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