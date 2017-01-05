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
import BDD._
import Compare.IndexResult
import Fingerprinting.FingerPrint
import Constellation.Spectrogram

import  javax.swing.border._
import java.awt.Color

object GUI extends SimpleSwingApplication {
  var time : Long = 0
  var readyWav : Boolean = false
  var readyBDD : Boolean = false
  val timeLabel : Label = new Label {}
  val resultLabel : Label = new Label {
    text = "Veuillez selectionner un fichier a analyser"
    font = new Font("SansSerif", Font.PLAIN, 18)
    border = new LineBorder(Color.BLACK)
  }
	val directoryAndFileLabel : Label = new Label {}
	var filePath : String = ""
	var directoryFilesName : Array[String] = DirectoryFilesList()
  val startButton : Button =	new Button {
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	CreateFoldersAndCache()
	
	def top = new MainFrame {
	  var width : Int = 700
  	val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  	val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
    val directoryFontMetrics : FontMetrics = peer.getFontMetrics(directoryAndFileLabel.font)
    val widthHtml : Int = directoryFontMetrics.stringWidth("</html>")
    
  	menuBar = new MenuBar {
  	  contents += new Menu("Options") {
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        var directoryBrowser = new JFileChooser(new File("."))
	        directoryBrowser.setDialogTitle("Choisissez le dossier contenant les 3 dossiers de frequences differentes")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
				  //Ouverture du selecteur de dossier avec verification que l'user appuie sur ok
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            var directoryBrowserAbsolutePath : String = directoryBrowser.getSelectedFile().getAbsolutePath()
            
            if(directoryBrowserAbsolutePath != directoryPath) {
              SetReadyBDD(false)
              directoryPath = directoryBrowserAbsolutePath
              var widthDirectoryText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").head) - widthHtml
              
              if(widthDirectoryText + 50 >= width || widthDirectoryText + 50 <= width)
        	      RefreshWidth(widthDirectoryText)
        	    
              RefreshFoldersAndCacheDirectories()
              CreateFoldersAndCache()
              directoryFilesName = DirectoryFilesList()
              DirectoryAnalysisLaunch()
            } else errorOrInfoWindow("Le dossier choisi est le même que précédemment", "Information")
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
            var widthFileText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").last) - widthHtml
      
      	    if(widthFileText + 50 >= width || widthFileText + 50 <= width)
      	      RefreshWidth(widthFileText)
      	      
            resultLabel.text = "Attente du demarrage de l'analyse"
            SetReadyWav(true)
          }
				}
				
				//Reaction au clic sur le bouton de lancement d'analyse
		    case ButtonClicked(component)	if component == startButton => {
		      time = System.currentTimeMillis()
		      startButton.enabled = false
		      RefreshResult("Analyse en cours...")
		      var wav : Array[Double] = DownSamplingAndStereoToMono(WavAnalysis(filePath))
		      
		      if(hamming(0) == 0.0)
		        Hamming(sampleLength)
		      
		      var ID : Int = IndexResult(FingerPrint(Spectrogram(SplitingAndFFT(wav, wav.length, sampleLength), sampleLength, frequencyBase)), fingerPrintsDirectory)
		      
		      if(ID != -1) {
		        var songName : String = filesNames(ID)
		        RefreshResult("La musique est : " + songName.substring(0, songName.length() - 4))
		      } else RefreshResult("Musique inexistante dans la base de données")
		      
		      var widthResultText : Int = peer.getFontMetrics(resultLabel.font).stringWidth(resultLabel.text)
		      
		      if(widthResultText + 50 >= width || widthResultText + 50 <= width)
	          RefreshWidth(widthResultText)
	          
		      RefreshFinish()
		    }
  		}
  	}
  	
  	//Rafraichissement de la taille de la fenetre au cas ou l'ecriture serait plus grande que celle ci
  	def RefreshWidth(widthResultText : Int) {
  	  width = widthResultText + 50
      peer.setPreferredSize(new Dimension(width, 250))
      peer.repaint()
      peer.validate()
	  }
  	
  	title = "Shazam Like"
  	val icon : ImageIcon = new ImageIcon("shazam.png");
    peer.setIconImage(icon.getImage());
  	resizable = false
    peer.setPreferredSize(new Dimension(width, 250))
    peer.setLocationRelativeTo(null)
 	}

	//Affichage des messages d'erreur (ex : aucun fichier dans le dossier BDD)
	def errorOrInfoWindow(message : String, ErrorOrInfo : String) {JOptionPane.showMessageDialog(top.peer, message, ErrorOrInfo, JOptionPane.ERROR_MESSAGE)}
	
	//Affichage du temps ecoule pour l'analyse
	def RefreshTime(time : Float) {timeLabel.text = "Temps écoulé pour l'analyse : " + time.toString() + " secondes"}
	
	//Rafraichissement du l'ecriture du resultat
  def RefreshResult(result : String) {resultLabel.text = result}
  
  //Rafraichissement du chemin du dossier et du fichier
	def RefreshDirectoryAndFileText() {directoryAndFileLabel.text = "<html>Dossier d'analyse : " + directoryPath + "<br>" + "Fichier à  analyser : " + filePath.split("/").last + "</html>"}
	
	//Le fichier Wav est choisi
	def SetReadyWav(ready : Boolean) {
	  readyWav = ready
	  RefreshReadyBDD(readyBDD, readyWav)
	}
	
	//La BDD est prête
	def SetReadyBDD(ready : Boolean) {
	  readyBDD = ready
	  RefreshReadyBDD(readyBDD, readyWav)
	}
	
	//Rafraichissement du bouton de lancement une fois que la BDD est completement anlysee
	def RefreshReadyBDD(BDD : Boolean, wav : Boolean) {
	  if(readyBDD && readyWav) {
	    startButton.text = "Lancer l'analyse"
	    startButton.enabled = true
	  } else if(readyBDD && !readyWav) {
	    startButton.text = "Choisissez un fichier .wav a analyser"
	    startButton.enabled = false
	  } else {
		  startButton.text	=	"En attente de la fin d'analyse de la BDD..."
		  startButton.enabled = false
	  }
	}
	
	//Rafraichissement du bouton de lancement d'analyse
	def RefreshFinish() {
	  startButton.text =	"Relancer l'analyse"
	  startButton.enabled = true
	  RefreshTime((System.currentTimeMillis() - time) / 1000F)
	}
	
  DirectoryAnalysisLaunch()
}