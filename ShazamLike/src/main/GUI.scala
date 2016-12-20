package main

import scala.math._
import scala.swing._
import scala.swing.event._
import javax.swing._
import javax.swing.filechooser._
import java.io._
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Graphics
import FFT._
import Import._

object GUI extends SimpleSwingApplication {
  var time : Long = 0
  val timeLabel : Label = new Label {}
  val resultLabel : Label = new Label {
    text = "Veuillez selectionner un fichier a analyser"
    font = new Font("SansSerif", Font.PLAIN, 18)
  }
	val directoryAndFileLabel : Label = new Label {}
	var filePath : String = ""
	var errorMessage : String = "Attention, le dossier par defaut est vide ou ne contient pas que des fichiers .wav"
	var directoryPath : String = System.getProperty("user.dir") + "\\BDD"
	val cacheFile : File = new File("cache.txt")
	if(!cacheFile.exists())
	  cacheFile.createNewFile()
	var directoryFilesName : Array[String] = DirectoryFilesList(directoryPath)
  val startButton : Button =	new Button {
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	
	def top = new MainFrame {
	  var width : Int = 700
  	val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  	val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
    
  	menuBar = new MenuBar {                                                               //Barre d'options
  	  contents += new Menu("Options") {                                                   //Ajout d'un menu deroulant
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        val directoryBrowser = new JFileChooser(new File("."))
	        directoryBrowser.setDialogTitle("Choisir un dossier contenant des sons")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
				  //Ouverture du selecteur de dossier avec verification que l'user appuie sur ok
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            directoryPath = directoryBrowser.getSelectedFile().getAbsolutePath()
            directoryFilesName = DirectoryFilesList(directoryPath)
            DirectoryAnalysisLaunch(peer)
          }
  	    })
  	    
  	    contents += new Separator
  	    contents += new MenuItem(swing.Action("Quitter") {sys.exit()})
  	  }
  	}
  	
  	//Cree un panel de 6 lignes de boutons et de phrases
  	contents = new GridPanel(6, 1) {
  	  RefreshDirectoryAndFileText()
  	  
  	  contents += browseFileButton
  	  contents += directoryAndFileLabel
  	  contents += new Separator()
      contents += startButton
      contents += resultLabel
      contents += timeLabel
      browseFileButton.preferredSize = new Dimension(width, 50)
  	  startButton.preferredSize = new Dimension(width, 50)
  	  
  		listenTo(browseFileButton)
  		listenTo(startButton)
  		
  		reactions += {
  	    //Ouverture du selecteur de fichiers avec verification que l'utilisateur appuie sur OK et qu
				case ButtonClicked(component) if component == browseFileButton => {          
				  val fileBrowser : JFileChooser = new JFileChooser(new File("."))
				  fileBrowser.setFileSelectionMode(JFileChooser.FILES_ONLY)
				  fileBrowser.setDialogTitle("Choisir un son")
          fileBrowser.setAcceptAllFileFilterUsed(false)
          fileBrowser.addChoosableFileFilter(new FileNameExtensionFilter(".wav", "wav"))
				  
          if(fileBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION && IsDirectoryFiles()) {
              filePath = fileBrowser.getSelectedFile.getAbsolutePath()
              RefreshDirectoryAndFileText()
              resultLabel.text = "Attente du demarrage de l'analyse"
              startButton.text = "Lancer l'analyse"
              startButton.enabled = true
              println("Fichier à analyser: " + filePath)
          }
				}
				
				//Reaction au clic sur le bouton de lancement d'analyse
		    case ButtonClicked(component)	if component == startButton => {
		      time = System.currentTimeMillis()
		      resultLabel.text = "Analyse en cours..."
		      var wav2D : Array[Array[Int]] = WavAnalysis(filePath)
		      var parameters = wav2D(0)
		      println("FFT : " + SplitingAndFFT(IntToFloat(wav2D(1), parameters(2)), parameters, sampleLength)) /////////////////////////
          println("Frequence d'echantillonage : " + parameters(0))
          println("Canaux : " + parameters(1))
          println("Echantillon : " + parameters(2))
		      startButton.text =	"Analyse..."
		      startButton.enabled = false
		    }
  		}
  	  		    
	    /*
	     * A mettre au niveau de l'affichage du resultat final
	     * Suivi du refreshResultText()
	     */
  	  //Verification des tailles des chemins et reultat
	    var directoryFontMetrics : FontMetrics = peer.getFontMetrics(directoryAndFileLabel.font)
	    var widthHtml : Int = directoryFontMetrics.stringWidth("</html>")
	    var widthDirectoryText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").head) - widthHtml
	    var widthFileText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").last) - widthHtml
	    var widthResultText : Int = peer.getFontMetrics(resultLabel.font).stringWidth(resultLabel.text)
	    if(widthResultText + 50 >= width || widthResultText + 50 <= width)
	      RefreshWidth(widthResultText)
	    if(widthFileText + 50 >= width || widthFileText + 50 <= width)
	      RefreshWidth(widthFileText)
      if(widthDirectoryText + 50 >= width || widthDirectoryText + 50 <= width)
	      RefreshWidth(widthDirectoryText)
  	}
  	
  	//Rafraichissement de la taille de la fenetre au cas ou l'ecriture serait plus grande que celle ci
  	def RefreshWidth(widthResultText : Int) {
  	  width = widthResultText + 50
      peer.setPreferredSize(new Dimension(width, 200))
      peer.repaint()
      peer.validate()
	  }
  	
  	title = "Shazam Like"
  	val icon : ImageIcon = new ImageIcon("shazam.png");
    peer.setIconImage(icon.getImage());
  	resizable = false
    peer.setPreferredSize(new Dimension(width, 200))
    peer.setLocationRelativeTo(null)
    peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    pack()
    visible = true
    
    DirectoryAnalysisLaunch(peer)
	}
	
	//Ensemble des verifications avant le lancement de l'analyse de la BDD
	def DirectoryAnalysisLaunch(peer : java.awt.Component) {
	  if(IsModif(new File(directoryPath))) {
  	  if(IsDirectoryFilesAndWav(directoryFilesName, peer))
        return DirectoryFilesAnalysis(directoryFilesName, directoryPath)
      else errorMessageWindow(peer, errorMessage)
	  } else errorMessageWindow(peer, "Le dossier sélectionné est le même que pour la dernière utilisation")
  }
	
	//Affichage des messages d'erreur (ex : aucun fichier dans le dossier BDD)
	def errorMessageWindow(peer : java.awt.Component, message : String) {JOptionPane.showMessageDialog(peer, message, "Erreur", JOptionPane.ERROR_MESSAGE)}
	
	//Verifie que le dossier BDD ne contient que des fichiers wav
	def IsDirectoryFilesAndWav(files : Array[String], peer : java.awt.Component) : Boolean = {
	  if(IsDirectoryFiles()) {
      for(i <- 0 to files.length - 1) {
        if(!files(i).toString().endsWith(".wav")) {
          errorMessage = "Veuillez sélectionner un dossier ne contenant que des fichiers .wav"
          return false
        }
	    }
	    return true
	  } else {
	    errorMessage = "Ce dossier ne contient aucun fichier"
	    return false
	  }
	}
	
	//Verifie si le dossier BDD a ete modifie ou pas pour ne pas faire l'analyse 2 fois
	def IsModif(directory : File) : Boolean = {
	  /*
	  var date = directory.lastModified().toString()
	  var cacheFileReader : String = new BufferedReader(new FileReader(cacheFile)).readLine()
	  if(date != cacheFileReader) {
	    var cacheFileWriter : BufferedWriter = new BufferedWriter(new FileWriter(cacheFile))
	    cacheFileWriter.write(date)
      cacheFileWriter.close()
      return true
	  } else return false
	  */
	  return true
	}
	
	//Verifie qu'il y ai des fichiers dans le dossier BDD
	def IsDirectoryFiles() : Boolean = {
	  if(directoryFilesName.length > 0)
	    return true
	  else return false
	}
	
	//Affichage du temps ecoule pour l'analyse
	def RefreshTime(time : Float) {timeLabel.text = "Temps écoulé pour l'analyse : " + time.toString() + " secondes"}

	//Rafraichissement du l'ecriture du resultat
  def RefreshResult(result : String) {resultLabel.text = result}

  //Rafraichissement du chemin du dossier et du fichier
	def RefreshDirectoryAndFileText() {directoryAndFileLabel.text = "<html>Dossier d'analyse : " + directoryPath + "<br>" + "Fichier à  analyser : " + filePath.split("/").last + "</html>"}

	//Rafraichissement du bouton de lancement d'analyse
	def RefreshFinish() {
	  startButton.text =	"Relancer l'analyse"
	  startButton.enabled = true
	  RefreshTime((System.currentTimeMillis() - time) / 1000F)
	}
}