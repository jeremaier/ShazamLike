package main

import java.io._
import java.util._
import GUI._
import Import._

object BDD {
  var directoryPath : String = System.getProperty("user.dir") + "\\BDD"
  var folders : Array[File] = Array[File](new File(directoryPath), new File(directoryPath + "\\11"), new File(directoryPath + "\\22"), new File(directoryPath + "\\44"))
	var cacheFiles : Array[File] = Array[File](new File(directoryPath + "\\cache.txt"), new File(folders(1).getAbsolutePath + "\\cache.txt"), new File(folders(2).getAbsolutePath + "\\cache.txt"), new File(folders(3).getAbsolutePath + "\\cache.txt"))
	val modif : Array[Boolean] = Array[Boolean](false, false, false, false)
  var errorMessage : String = "Attention, le dossier par defaut est vide ou ne contient pas que des fichiers .wav"
	      
  //Creation des fichiers et dossiers necessaires a l'analyse de BDD s'il n'existe pas
  def CreateFoldersAndCache() {
    for(i <- 0 to 3) {
  	  if(!folders(i).exists())
  	    folders(i).mkdirs()
  	  cacheFiles(i).createNewFile()
    }
	}
  
	//Verifie si le dossier BDD a ete modifie ou pas pour ne pas faire l'analyse 2 fois
	def IsModif() {
	  val dateBDD : Array[String] = new Array(4)
	  var dateMax : String = "0"
	  for(i <- 0 to 3) {
	    dateBDD(i) = folders(i).lastModified().toString()
	    if(dateBDD(0) < dateBDD(i))
	      dateMax = dateBDD(i)
	  }
	  dateBDD(0) = dateMax

  	val cacheBr : BufferedReader = new BufferedReader(new FileReader(cacheFiles(0)))
	  
	  if(dateBDD(0) != cacheBr.readLine()) {
	    modif(0) = true
  	  for(i <- 1 to 3) {
  	    var date : String = cacheBr.readLine()
    	  if(dateBDD(i) != date)
          modif(i) = true
  	  }
	  }
	  
	  cacheBr.close()
	  
	  val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(cacheFiles(0)))
	  
	  for(i <- 0 to 3) {
	    cacheBw.append(dateBDD(i))
      cacheBw.newLine()
	  }
	  
	  cacheBw.close()
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
	  //for (int i = 0; i < AMOUNT_OF_POINTS; i++) {
    //fw.append(recordPoints[i] + "\t");
    //}
	  //Ecrire a la suite
	  //val cacheBw : BufferedWriter = new BufferedWriter(new FileWriter(cacheFiles(0), true))

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
	  IsModif()
	  
	  if(modif(0)) {
	    for(i <- 1 to 3) {
	      if(modif(i)) {
      	  if(IsDirectoryFilesAndWav(i - 1, peer))
      	    DirectoryFilesAnalysis(directoryFilesName(i - 1), directoryPath, i - 1, i match {case 3 => 4096
      	                                                                                     case _ => i * 1024})
      	  else errorMessageWindow(peer, errorMessage)
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
	  folders = Array[File](new File(directoryPath), new File(directoryPath + "\\11"), new File(directoryPath + "\\22"), new File(directoryPath + "\\44"))
	  cacheFiles = Array[File](new File(directoryPath + "\\cache.txt"), new File(folders(1).getAbsolutePath + "\\cache.txt"), new File(folders(2).getAbsolutePath + "\\cache.txt"), new File(folders(3).getAbsolutePath + "\\cache.txt"))
	}
}