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

object GUI extends SimpleSwingApplication {
  var time : Long = 0
  val timeLabel : Label = new Label {}
  val resultLabel : Label = new Label {
    text = "Veuillez selectionner un fichier a analyser"
    font = new Font("SansSerif", Font.PLAIN, 18)
  }
	val directoryAndFileLabel : Label = new Label {}
	var filePath : String = ""
	var directoryPath : String = System.getProperty("user.dir") + "\\BDD"
	var cacheFile : File = new File("cache.txt")
	if(!cacheFile.exists()) cacheFile.createNewFile()
	var directoryFilesName : Array[String] = Import.DirectoryFilesList(directoryPath)
  val startButton : Button =	new Button {                                              //Bouton pour lancer l'analyse
		text	=	"Choisissez un fichier .wav a analyser"
		enabled = false
	}
	
	def top = new MainFrame {
	  var width : Int = 700
  	val browseFileButton : Button =	new Button	{text	=	"Parcourir..."}
  	val browseDirectoryButton : Button =	new Button	{text	=	"Changer de dossier..."}
    
    if(IsDirectoryFilesAndWav(directoryFilesName, "Aucun fichier dans le dossier par defaut", peer) && !IsModif(new File(directoryPath)))
      Import.DirectoryFilesAnalysis(directoryFilesName, directoryPath)                    //Lancement de l'analyse BDD
    else JOptionPane.showMessageDialog(peer, "Attention, le dossier par defaut est vide ou ne contient pas que des fichiers .wav", "Erreur", JOptionPane.ERROR_MESSAGE)
    
  	menuBar = new MenuBar {                                                               //Barre d'options
  	  contents += new Menu("Options") {                                                   //Ajout d'un menu deroulant
  	    contents += new MenuItem(swing.Action("Changer de dossier...") {
	        val directoryBrowser = new JFileChooser(new File("."))
	        directoryBrowser.setDialogTitle("Choisir un dossier contenant des sons")
				  directoryBrowser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
				  
          if(directoryBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {      //Ouverture du selecteur de dossier avec vï¿½rification que l'user appuie sur ok
            var directory : File = directoryBrowser.getSelectedFile()
            directoryPath = directory.getAbsolutePath()
            directoryFilesName = Import.DirectoryFilesList(directoryPath)

            if(IsDirectoryFilesAndWav(directoryFilesName, "Aucun fichier dans ce dossier", peer) && !IsModif(directory))
              Import.DirectoryFilesAnalysis(directoryFilesName, directoryPath)                //Lancement de l'analyse BDD
            else JOptionPane.showMessageDialog(peer, "Veuillez selectionner un dossier non vide ne contenant que des fichiers .wav", "Erreur", JOptionPane.ERROR_MESSAGE)
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
				  
          if(fileBrowser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION && IsDirectoryFiles()) {      //Ouverture du selecteur de fichiers avec vï¿½rification que l'utilisateur appuie sur OK
              filePath = fileBrowser.getSelectedFile.getAbsolutePath()
              RefreshDirectoryAndFileText()
              resultLabel.text = "Attente du demarrage de l'analyse"
              startButton.text = "Lancer l'analyse"
              startButton.enabled = true
              println("Fichier à analyser: " + filePath)
          }
				}
				
		    case ButtonClicked(component)	if component == startButton => {               //Lance l'analyse du fichier
		      time = System.currentTimeMillis()
		      resultLabel.text = "Analyse en cours..."
		      var wav2D : Array[Array[Int]] = Import.WavAnalysis(filePath)
		      var fft = FFT.FileFFT(wav2D(1), wav2D(0)(2), false)
		      print("FFT : " + fft)            ////////////////////////////////////////////////////////////////////////////
          println("Frequence d'echantillonage : " + wav2D(0)(0))
          println("Canaux : " + wav2D(0)(1))
          println("Echantillon : " + wav2D(0)(2))
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
		    if(widthResultText + 50 >= width || widthResultText <= width - 50 || widthFileText + 50 >= width || widthFileText <= width - 50 || widthDirectoryText + 50 >= width || widthDirectoryText <= width - 50)                    //Vï¿½rifie que les noms ne soient pas plus grand que la fenï¿½tre
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
	
	def IsDirectoryFilesAndWav(files : Array[String], message : String, peer : java.awt.Component) :Boolean = {
	  if(IsDirectoryFiles()) {
      for(i <- 0 to files.length - 1)                 //Verifie que tous les fichiers sont en .wav
        if(!files(i).toString().endsWith(".wav")) return false
  	  return true
	  } else return false
	}
	
	def IsModif(directory : File) : Boolean = {
	  var date = directory.lastModified().toString()
	  var cacheFileReader : String = new BufferedReader(new FileReader(cacheFile)).readLine()
	  println(date + ", " + cacheFileReader)  ///////////////////
	  if(date != cacheFileReader) {
	    var cacheFileWriter : BufferedWriter = new BufferedWriter(new FileWriter(cacheFile))
	    cacheFileWriter.write(date)
      cacheFileWriter.close()
      return true
	  } else return false
	}
	
	def IsDirectoryFiles() : Boolean = {
	  if(directoryFilesName.length > 0) return true
	  else  return false
	}
	
	def RefreshTime(time : Float) {timeLabel.text = "Temps écoulé pour l'analyse : " + time.toString() + " secondes"}

  def RefreshResult(result : String) {resultLabel.text = result}

	def RefreshDirectoryAndFileText() {directoryAndFileLabel.text = "<html>Dossier d'analyse : " + directoryPath + "<br>" + "Fichier à  analyser : " + filePath.split("/").last + "</html>"}

	def RefreshFinish() {
	  startButton.text =	"Relancer l'analyse"
	  startButton.enabled = true
	  RefreshTime((System.currentTimeMillis() - time) / 1000F)
	}
}