/**
 * Author: Peter Started:01.05.2011
 */
package client.print
import scala.swing.Window
import scala.swing.Dialog
import java.awt.Dimension
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Button
import java.awt.Color
import scala.swing.BorderPanel
import javax.swing.BorderFactory
import scala.swing.Label
import scala.swing.RadioButton
import scala.swing.ButtonGroup
import scala.swing.TextField
import scala.swing.ScrollPane
import scala.swing.event.ButtonClicked
import scala.swing.Component
import javax.swing.JOptionPane
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.ImageIcon
import java.net.URL
import definition.data.Reference
import javax.print.attribute.standard.PageRanges

/**
 * 
 */
class PrintOutDialog (preDialog:NewOutdefDialog)  extends BoxPanel(Orientation.Vertical){
  preferredSize=new Dimension(350,400)  
  
  var pageRange:PageRanges=_
  
  val dlabel=new Label()
  val plabel=new Label()
  val allPagesBut=new RadioButton("Alle Seiten drucken")
  val pageRangeBut=new RadioButton("Seitenbereich:")
  val currentPageBut=new RadioButton("Aktuelle Seite drucken")
  val pagesGroup=new ButtonGroup(allPagesBut,currentPageBut,pageRangeBut)
  val pageRangeEdit=new TextField
  
  pageRangeEdit.peer .getDocument.addDocumentListener(new DocumentListener{
  	def changedUpdate(e:DocumentEvent)= checkPages
  	def insertUpdate(e:DocumentEvent)= checkPages
  	def removeUpdate(e:DocumentEvent)= checkPages
  })
  
  val addReceiverBut=new Button("Hinzufügen...")
  
  var currPageable:APageable= _
  var currPage:Int = 0
  
  var printerIcon=createImageIcon("printer-icon_small.png")
  
  val outputPane=new BoxPanel(Orientation.Vertical){  	
  	border=BorderFactory.createTitledBorder("Ausgabe auf")
  	contents+=new BoxPanel(Orientation.Horizontal ){
  	 val olabel=new Label("Gerät: ")
  	 contents+=olabel+=dlabel
  	 xLayoutAlignment=0
  	}
  	contents+=new BoxPanel(Orientation.Horizontal ){
  	 val olabel=new Label("Papier: ")
  	 contents+=olabel+=plabel
  	 xLayoutAlignment=0
  	}
  	xLayoutAlignment=0  	
  	maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
  }
  
  val pagesPane= new BoxPanel(Orientation.Vertical){  	
  	xLayoutAlignment=0
  	border=BorderFactory.createTitledBorder("Druckbereich")
  	val exampleLab=new Label(" z.B.: 1-5, 8, 11 ")
  	exampleLab.foreground=Color.gray
  	val rangeBox=new BoxPanel(Orientation.Horizontal ){
  		contents+=pageRangeBut+=pageRangeEdit+=exampleLab
  	}
  	allPagesBut.xLayoutAlignment=0
  	currentPageBut.xLayoutAlignment=0
  	rangeBox.xLayoutAlignment=0
  	contents+=allPagesBut+=rangeBox+=currentPageBut  
  	maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
  	allPagesBut.selected=true
  }
  
  val receiverPan=new BorderPanel(){
  	xLayoutAlignment=0
  	border=BorderFactory.createTitledBorder("Empfänger")
  	val receiverScroller=new ScrollPane (){
  		preferredSize=new Dimension(100,50)
  	}
  	add(receiverScroller,BorderPanel.Position.Center )
  	val managePanel=new BoxPanel(Orientation.Vertical ){
  		contents+=addReceiverBut
  	}
  	add(managePanel,BorderPanel.Position.Center )
  }
  
  
  contents+=outputPane+=pagesPane+=receiverPan  
  
  listenTo(addReceiverBut)
  
  reactions += {
  	 case ButtonClicked(`addReceiverBut`)=> 
  }
  
  def checkPages= {
  	if(currPageable!=null) 
  	pageRangeEdit.foreground=if(parsePages(pageRangeEdit.text)==null)Color.red else Color.black
  	if(!pageRangeBut.selected) pageRangeBut.selected=true
  }
  
  def parsePages(pageSt:String):PageRanges= {   
  	try {
  		return new PageRanges(pageSt)
  	}
  	catch {case _=> return null }
  }
  
  def getPages= {
  	if(currPageable==null) null
  	else if(allPagesBut.selected) new PageRanges(1, currPageable.getNumberOfPages)
  	else if(pageRangeBut.selected) new PageRanges(pageRangeEdit.text)
  	else new PageRanges(currPage)
  }
  
  
  
  def showPrintOutDialog(parent:Component,nPageable:APageable,ncurrPage:Int):(PageRanges,Seq[Reference])={
  	currPageable=nPageable
  	currPage=ncurrPage
  	dlabel.text=preDialog.printerName 
  	plabel.text=preDialog.lastSelectedMedia.toString 
  	val dr="Drucken"
  	if (JOptionPane.YES_OPTION== JOptionPane.showConfirmDialog(parent.peer,peer,"Druckausgabe",Dialog.Options.OkCancel.id,
  		Dialog.Message.Question.id,printerIcon))  	
  		(getPages,Seq.empty)
  	else (null,Seq.empty)
  }
  
  
  def  createImageIcon(path:String):ImageIcon = {
		val imgURL:URL   = this.getClass.getResource(path)
		if (imgURL != null) {
			null
			return new ImageIcon(imgURL);
		} else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}
  
}