package main

import scala.math._
import scala.swing._
import scala.swing.event._
import javax.swing._
import javax.swing.filechooser._
import javax.swing.border._
import java.io._
import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Graphics
import FFT._
import Import._
import BDD._
import Compare.BestMatching
import Fingerprinting.FingerPrint
import Constellation.Spectrogram

import java.util.regex._

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
	var currentFolder : String = "."
	var directoryFilesName : Array[String] = DirectoryFilesList()
  val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
  val startButton : Button =	new Button {
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	var topPeer = null
	CreateFoldersAndCache()
	
	//Creation de la fenetre ainsi que tout ce qui la constitue
	def top = new MainFrame {
  	menuBar = new MenuBar {
  	  contents += new Menu("Options") {
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        var directoryBrowser = new JFileChooser(new File(currentFolder))
	        directoryBrowser.setDialogTitle("Choisissez le dossier contenant les 3 dossiers de frequences differentes")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
				  //Ouverture du selecteur de dossier avec verification que l'user appuie sur ok
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            var directoryBrowserAbsolutePath : String = directoryBrowser.getSelectedFile().getAbsolutePath()
            
            if(directoryBrowserAbsolutePath != directoryPath) {
              currentFolder = directoryBrowserAbsolutePath
              SetReadyBDD(false)
              directoryPath = directoryBrowserAbsolutePath
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
  	
  	//Cree un panel de 6 lignes de boutons et de labels
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
				  val fileBrowser : JFileChooser = new JFileChooser(new File(currentFolder))
				  fileBrowser.setFileSelectionMode(JFileChooser.FILES_ONLY)
				  fileBrowser.setDialogTitle("Choisir un son")
          fileBrowser.setAcceptAllFileFilterUsed(false)
          fileBrowser.addChoosableFileFilter(new FileNameExtensionFilter(".wav", "wav"))
				  
          if(fileBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION && IsDirectoryFiles()) {
            filePath = fileBrowser.getSelectedFile.getAbsolutePath()
            currentFolder = filePath.substring(0, filePath.lastIndexOf("\\"))
            RefreshDirectoryAndFileText()
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
		      
		      var ID : Int = BestMatching(FingerPrint(Spectrogram(SplitingAndFFT(wav, wav.length, sampleLength), frequencyBase, sampleLength)), fingerPrintsDirectory)
		      
		      if(ID != -1) {
		        var songName : String = filesNames(ID)
		        RefreshResult("La musique est : " + songName.substring(0, songName.length() - 12))
		      } else RefreshResult("Musique inexistante dans la base de données")

		      RefreshFinish()
		    }
  		}
  	}
  	
  	title = "Shazam"
  	val icon : ImageIcon = new ImageIcon("shazam.png")
    peer.setIconImage(icon.getImage())
    peer.setSize(800, 300)
  	resizable = false
  	visible = true
    peer.setLocationRelativeTo(null)
    var topPeer = this.peer
 	}

	//Affichage des messages d'erreur (ex : aucun fichier dans le dossier BDD)
	def errorOrInfoWindow(message : String, ErrorOrInfo : String) {JOptionPane.showMessageDialog(topPeer, message, ErrorOrInfo, JOptionPane.ERROR_MESSAGE)}
	
	//Affichage du temps ecoule pour l'analyse
	def RefreshTime(time : Float) {timeLabel.text = "Temps écoulé pour l'analyse : " + time.toString() + " secondes"}
	
	//Rafraichissement du l'ecriture du resultat
  def RefreshResult(result : String) {resultLabel.text = result}
  
  //Rafraichissement du chemin du dossier et du fichier
	def RefreshDirectoryAndFileText() {directoryAndFileLabel.text = "<html>Dossier d'analyse : " + directoryPath + "<br>" + "Fichier à  analyser : " + filePath.substring(filePath.lastIndexOf("\\") + 1, filePath.length()) + "</html>"}
	
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