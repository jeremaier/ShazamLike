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

object GUI extends SimpleSwingApplication {
  var time : Long = 0
  val timeLabel : Label = new Label {}
  val resultLabel : Label = new Label {
    text = "Veuillez selectionner un fichier a analyser"
    font = new Font("SansSerif", Font.PLAIN, 18)
  }
	val directoryAndFileLabel : Label = new Label {}
	var filePath : String = ""
	var directoryFilesName : Array[Array[String]] = DirectoryFilesList()
  val startButton : Button =	new Button {
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	
	CreateFoldersAndCache()
	
	def top = new MainFrame {
	  var width : Int = 700
  	val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  	val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
    
  	menuBar = new MenuBar {                                                               //Barre d'options
  	  contents += new Menu("Options") {                                                   //Ajout d'un menu deroulant
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        val directoryBrowser = new JFileChooser(new File("."))
	        directoryBrowser.setDialogTitle("Choisissez le dossier contenant les 3 dossiers de frequences differentes")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
				  //Ouverture du selecteur de dossier avec verification que l'user appuie sur ok
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            directoryPath = directoryBrowser.getSelectedFile().getAbsolutePath()
            RefreshFoldersAndCacheDirectories()
            CreateFoldersAndCache()
            directoryFilesName = DirectoryFilesList()
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
		      var parameters : Array[Int] = wav2D(0)
		      println("FFT : " + SplitingAndFFT(IntToDouble(wav2D(1), parameters(2)), parameters(2), sampleLength)) /////////////////////////
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
	
	//Affichage des messages d'erreur (ex : aucun fichier dans le dossier BDD)
	def errorMessageWindow(peer : java.awt.Component, message : String) {JOptionPane.showMessageDialog(peer, message, "Erreur", JOptionPane.ERROR_MESSAGE)}
	
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