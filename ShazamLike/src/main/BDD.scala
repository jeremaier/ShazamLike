package main

import java.io._
import java.util._
import GUI._
import Import._

object BDD {
  var directoryPath : String = System.getProperty("user.dir") + "\\BDD"
  var folders : Array[File] = Array(new File(directoryPath), new File(directoryPath + "\\11"), new File(directoryPath + "\\22"), new File(directoryPath + "\\44"))
	var cacheFiles : Array[File] = Array(new File(directoryPath + "\\cache.txt"), new File(folders(1).getAbsolutePath + "\\cache.txt"), new File(folders(2).getAbsolutePath + "\\cache.txt"), new File(folders(3).getAbsolutePath + "\\cache.txt"))
	val modif : Array[Boolean] = Array(false, false, false, false)
  var errorMessage : String = "Attention, le dossier par defaut est vide ou ne contient pas que des fichiers .wav"
	
  //Creation des fichiers et dossiers necessaires a l'analyse de BDD s'il n'existe pas
  def CreateFoldersAndCache() {
    for(i <- 0 to 3) {
  	  if(!folders(i).exists()) {
  	    folders(i).mkdir()
  	    cacheFiles(i).createNewFile()
  	  }
    }
	  cacheFiles(0).createNewFile()
	}
  
	//Verifie si le dossier BDD a ete modifie ou pas pour ne pas faire l'analyse 2 fois
	def IsModif() {
	  val dateBDD : Array[String] = Array()
	  for(i <- 0 to 3)
	    dateBDD(i) = folders(i).lastModified().toString()

  	val cacheBr : BufferedReader = new BufferedReader(new FileReader(dateBDD(0)))
  	val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(folders(0)))
	  for(i <- 0 to 3) {
  	  if(dateBDD(i) != cacheBr.readLine()) {
  	    cacheBw.append(dateBDD(i))
        cacheBw.newLine()
        modif(i) = true
  	  }
	  }
	  cacheBw.close()
	  cacheBr.close()
	}
	
	//Reinitialisation de la liste des dossiers qui ont ete modifie
	def modifReset() {
	 for(i <- 0 to 3)
     modif(i) = false
	}
	
	//Va lire le contenu de chaque fichier cache
	def CacheReader(i : Int) {
  	val cacheS : Scanner = new Scanner(cacheFiles(i))
	  while (true)
    {
        object allDone extends Exception {}
        try {
          for(i <- 0 to 4) {
        	  for(j <- 0 to 4)
        	    cacheS.nextDouble()
        	}
        } catch {case allDone : Throwable =>}
    }
	  cacheS.close()
	}
	
	//Va ecrire le resultat des analyses de BDD precedentes
	def CacheWriter(i : Int) {
  	val cachePw : PrintWriter = new PrintWriter(cacheFiles(i))
    for(i <- 0 to 4) {
  	  for(j <- 0 to 3)
  	    cachePw.print()
  	  cachePw.println()
  	}
	  cachePw.close()
	}
  
	//Ensemble des verifications avant le lancement de l'analyse de la BDD
	def DirectoryAnalysisLaunch(peer : java.awt.Component) {
	  if(modif(0)) {
	    for(i <- 1 to 3) {
	      if(modif(i)) {
      	  if(IsDirectoryFilesAndWav(i, peer)) {
      	    sampleLength = i * 1024
      	    DirectoryFilesAnalysis(directoryFilesName(i), directoryPath)
      	  } else errorMessageWindow(peer, errorMessage)
	      } else {
	        var ModifErrorMessage : String = "Le dossier des sons de 11kHz est le même que pour la dernière utilisation"
	        i match {
            case 2 => ModifErrorMessage = "Le dossier des sons de 22kHz est le même que pour la dernière utilisation"
            case 3 => ModifErrorMessage = "Le dossier des sons de 44kHz est le même que pour la dernière utilisation"
            case _ => ModifErrorMessage = "Le dossier des sons de 11kHz est le même que pour la dernière utilisation"
          }
	        errorMessageWindow(peer, ModifErrorMessage)
	      }
	    }
	    modifReset()
	  } else {
	    errorMessageWindow(peer, "Aucun dossier modifié depuis la dernière utilisation")
	    ////////////////////////////////////////////LECTURE DES FICHIERS CACHE
	  }
  }
	
	//Verifie que le dossier BDD ne contient que des fichiers wav
	def IsDirectoryFilesAndWav(folderNumber : Int, peer : java.awt.Component) : Boolean = {
	  val files : Array[String] = directoryFilesName(folderNumber)
	  
	  if(IsDirectoryFiles()) {
      for(i <- 0 to files.length - 1) {
        if(!files(i).toString().endsWith(".wav")) {
          folderNumber match {
            case 2 => errorMessage = "Le dossier des sons de 22kHz ne contient pas que des fichiers .wav"
            case 3 => errorMessage = "Le dossier des sons de 44kHz ne contient pas que des fichiers .wav"
            case _ => errorMessage = "Le dossier des sons de 11kHz ne contient pas que des fichiers .wav"
          }
          return false
        }
	    }
	    return true
	  } else {
	    folderNumber match {
        case 2 => errorMessage = "Le dossier des sons de 11kHz ne contient aucun fichier"
        case 3 => errorMessage = "Le dossier des sons de 11kHz ne contient aucun fichier"
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
	  folders = Array(new File(directoryPath), new File(directoryPath + "\\11"), new File(directoryPath + "\\22"), new File(directoryPath + "\\44"))
	  cacheFiles = Array(new File(directoryPath + "\\cache.txt"), new File(folders(1).getAbsolutePath + "\\cache.txt"), new File(folders(2).getAbsolutePath + "\\cache.txt"), new File(folders(3).getAbsolutePath + "\\cache.txt"))
	}
}