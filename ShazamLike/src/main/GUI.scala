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
	var cacheFile : File = new File("cache.txt")
	if(!cacheFile.exists())
	  cacheFile.createNewFile()
	var directoryFilesName : Array[String] = DirectoryFilesList(directoryPath)
  val startButton : Button =	new Button {                                              //Bouton pour lancer l'analyse
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	
	def top = new MainFrame {
	  var width : Int = 700
  	val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  	val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
    
    DirectoryAnalysisLaunch(peer)
    
  	menuBar = new MenuBar {                                                               //Barre d'options
  	  contents += new Menu("Options") {                                                   //Ajout d'un menu deroulant
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        val directoryBrowser = new JFileChooser(new File("."))
	        directoryBrowser.setDialogTitle("Choisir un dossier contenant des sons")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {      //Ouverture du selecteur de dossier avec v�rification que l'user appuie sur ok
            var directory : File = directoryBrowser.getSelectedFile()
            directoryPath = directory.getAbsolutePath()
            directoryFilesName = DirectoryFilesList(directoryPath)
            DirectoryAnalysisLaunch(peer)
          }
  	    })
  	    
  	    contents += new Separator
  	    contents += new MenuItem(swing.Action("Quitter") {sys.exit()})
  	  }
  	}
  	
  	contents = new GridPanel(6, 1) {                                                 //Cree un panel de boutons et de phrase
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
				case ButtonClicked(component) if component == browseFileButton => {          
				  val fileBrowser : JFileChooser = new JFileChooser(new File("."))
				  fileBrowser.setFileSelectionMode(JFileChooser.FILES_ONLY)
				  fileBrowser.setDialogTitle("Choisir un son")
          fileBrowser.setAcceptAllFileFilterUsed(false)
          fileBrowser.addChoosableFileFilter(new FileNameExtensionFilter(".wav", "wav"))
				  
          if(fileBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION && IsDirectoryFiles()) {      //Ouverture du selecteur de fichiers avec v�rification que l'utilisateur appuie sur OK
              filePath = fileBrowser.getSelectedFile.getAbsolutePath()
              RefreshDirectoryAndFileText()
              resultLabel.text = "Attente du demarrage de l'analyse"
              startButton.text = "Lancer l'analyse"
              startButton.enabled = true
              println("Fichier � analyser: " + filePath)
          }
				}
				
		    case ButtonClicked(component)	if component == startButton => {               //Lance l'analyse du fichier
		      time = System.currentTimeMillis()
		      resultLabel.text = "Analyse en cours..."
		      var wav2D : Array[Array[Int]] = WavAnalysis(filePath)
		      var longueurWav = wav2D(0)(2)
		      println("FFT : " + ModuleFFT(FillFile(wav2D(1), longueurWav), longueurWav))
          println("Frequence d'echantillonage : " + wav2D(0)(0))
          println("Canaux : " + wav2D(0)(1))
          println("Echantillon : " + longueurWav)
		      startButton.text =	"Analyse..."
		      startButton.enabled = false
		    }
		    
		    /*
		     * A mettre au niveau de l'affichage du resultat final
		     * Suivi du refreshResultText()
		     */
		    var directoryFontMetrics : FontMetrics = peer.getFontMetrics(directoryAndFileLabel.font)
		    var widthHtml : Int = directoryFontMetrics.stringWidth("</html>")
		    var widthDirectoryText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").head) - widthHtml
		    var widthFileText : Int = directoryFontMetrics.stringWidth(directoryAndFileLabel.text.split("<br>").last) - widthHtml
		    var widthResultText : Int = peer.getFontMetrics(resultLabel.font).stringWidth(resultLabel.text)
		    if(widthResultText + 50 >= width || widthResultText <= width - 50 || widthFileText + 50 >= width || widthFileText <= width - 50 || widthDirectoryText + 50 >= width || widthDirectoryText <= width - 50)                    //V�rifie que les noms ne soient pas plus grand que la fen�tre
		      RefreshWidth(widthResultText)
  		}
  	}
  	
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
	}
	
	def DirectoryAnalysisLaunch(peer : java.awt.Component) {
	  if(IsModif(new File(directoryPath))) {
  	  if(IsDirectoryFilesAndWav(directoryFilesName, peer))
        return DirectoryFilesAnalysis(directoryFilesName, directoryPath)
      else errorMessageWindow(peer, errorMessage)
	  } else errorMessageWindow(peer, "Le dossier s�lectionn� est le m�me que pour la derni�re utilisation")
  }
	
	def errorMessageWindow(peer : java.awt.Component, message : String) {JOptionPane.showMessageDialog(peer, message, "Erreur", JOptionPane.ERROR_MESSAGE)}
	
	def IsDirectoryFilesAndWav(files : Array[String], peer : java.awt.Component) : Boolean = {
	  if(IsDirectoryFiles()) {
      for(i <- 0 to files.length - 1) {                 //Verifie que tous les fichiers sont en .wav
        if(!files(i).toString().endsWith(".wav")) {
          errorMessage = "Veuillez s�lectionner un dossier ne contenant que des fichiers .wav"
          return false
        }
	    }
	    return true
	  } else {
	    errorMessage = "Ce dossier ne contient aucun fichier"
	    return false
	  }
	}
	
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
	
	def IsDirectoryFiles() : Boolean = {
	  if(directoryFilesName.length > 0) return true
	  else return false
	}
	
	def RefreshTime(time : Float) {timeLabel.text = "Temps �coul� pour l'analyse : " + time.toString() + " secondes"}

  def RefreshResult(result : String) {resultLabel.text = result}

	def RefreshDirectoryAndFileText() {directoryAndFileLabel.text = "<html>Dossier d'analyse : " + directoryPath + "<br>" + "Fichier � analyser : " + filePath.split("/").last + "</html>"}

	def RefreshFinish() {
	  startButton.text =	"Relancer l'analyse"
	  startButton.enabled = true
	  RefreshTime((System.currentTimeMillis() - time) / 1000F)
	}
}