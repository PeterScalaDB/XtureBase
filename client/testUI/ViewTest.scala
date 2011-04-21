/**
 * Author: Peter Started:05.09.2010
 */
package client.testUI

import scala.swing._
import scala.swing.event._
import javax.swing._
import server.storage._
import server.config._
import transaction.handling._
import definition.data._
import definition.typ.DataType
import definition.expression._
import client.model._
import client.comm._
import java.net._
import java.io._
import javax.swing.border._
import scala.collection.immutable.IndexedSeq
import client.layout._
import client.dataviewer._
import client.dialog._
import client.graphicsView._

/** tests the instance dataviewer
 * 
 */
object ViewTest extends SimpleSwingApplication {
	var sock:ClientSocket=null	
	
	
	val actionPan= new ActionPanel
	
	val fieldEditPan=new FieldEditorsPanel
	//val newPanelArea=new NewPanelArea
	
	val undoBut = new Button("Undo")
	val hideBut=new ToggleButton("Hide")
	val undoDialog=new UndoDialog(top)
	
	val mainBox=new MainBox
	var startTableViewbox:Viewbox=null 
	var startContentBox:TableViewbox=null 
	
	val tableViewboxType=new ViewboxContentType(1,"table","T",()=>{
		new TableViewbox
	})
	val graphicsViewboxType=new ViewboxContentType(1,"graphics","G",()=>{
		new GraphicsViewbox
	})
	

	val mainPanel:BorderPanel=	new BorderPanel()  // main panel
	{			
		 
		//mainBox.preferredSize=new Dimension(300,300)
		add (mainBox,BorderPanel.Position.Center)
		
		add( new BorderPanel() {
			peer.putClientProperty("newPanel","IGNORE")
			add(new BoxPanel(scala.swing.Orientation.Vertical){
				contents+=new BorderPanel {
					add (new Label("Objekte erzeugen:"){preferredSize=new Dimension(40,30)},BorderPanel.Position.North)
					add (NewPanelArea,BorderPanel.Position.Center)
				}
				contents+=fieldEditPan
				
			},BorderPanel.Position.North)			
			add(new BorderPanel() {
				preferredSize=new Dimension(140,100)
				add(new Label("Funktionen:"){preferredSize=new Dimension(40,30)},BorderPanel.Position.North)
				add (new ScrollPane() {
					viewportView= actionPan				
				},BorderPanel.Position.Center)
			},BorderPanel.Position.Center)
		},BorderPanel.Position.West)
		add( new BoxPanel(Orientation.Horizontal) {
			contents+= (new BoxPanel(Orientation.Vertical) { 
				contents+=undoBut+=hideBut
				}) +=Swing.HStrut(60)+=DialogManager.dialogPanel
		},BorderPanel.Position.South)
		
		listenTo(undoBut,hideBut)
		reactions += {					
					case ButtonClicked(`undoBut`) => requestUndoData
					case ButtonClicked(`hideBut`) => setHide(hideBut.selected)
		}
	}
	
	val top:MainFrame = new MainFrame ()
	{
		override def closeOperation() 
		{
			System.out.println("Close " )			
			shutDown()
			super.closeOperation()			
			
		}
		title="TableTest"
		contents = mainPanel
		//UIManager.put("Table.alternateRowColor", new Color(250,250,240));
	  //SwingUtilities.updateComponentTreeUI(this.peer);
		bounds=new Rectangle(20,20,1500,900)
		
	}	
	
	
	
	override def startup(args: Array[String]):Unit = {		
		if(args.length<4 ) { System.out.println("Usage: ViewTest host portnr name password [hide=1]"); quit() }		
		top.title= "Table Test ["+args(2)+"]"
		// connect components
		
		ClientQueryManager.registerAfterListener (() => {
			ViewboxContentTypeList.addType(tableViewboxType)
			ViewboxContentTypeList.addType(graphicsViewboxType)
			startTableViewbox=new Viewbox(mainBox,false,mainBox)
			startContentBox= new TableViewbox
			startTableViewbox.addContent(startContentBox)
			
			mainBox.centerBox=startTableViewbox
		}	)
		
		actionPan.registerActionPanListener(DialogManager)
		NewPanelArea.registerActionPanListener(DialogManager)
		SelectEventDispatcher.registerSelectListener(actionPan)
		SelectEventDispatcher.registerSelectListener(fieldEditPan)
		SelectEventDispatcher.registerSelectListener(DialogManager)		
		
		DialogManager.answerArea.registerCustomPanel[PointAnswerPanel](DataType.VectorTyp)
		ClientQueryManager.registerStepListReader(undoDialog)
		//System.out.println(top.title)
		val serverName=	if(args(0).toLowerCase=="[own]"){
			SessionManager.init()
			Thread.`yield`()
			Thread.sleep(100)
			"localhost"			
		} else args(0)
		sock=new ClientSocket(InetAddress.getByName(serverName),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()  	
  	super.startup(args)
  	if(args.size>4){
  		val sel:Boolean=args(4).toInt>0
  		if(sel)hideBut.selected=true
  		setHide(sel)
  	}
	}
	
	def shutDown() = {
		System.out.println("shutdown")
		mainBox.storeSettings()
		sock.quitApplication()
	}
	
	def requestUndoData = {
		undoDialog.setLocationRelativeTo(undoBut)
		undoDialog.size=new Dimension(600,300)
		
		//undoDialog.visible=true		
		ClientQueryManager.requestUndoData()
	}
	
	def setHide(value:Boolean) = {
		if(value){
			hideBut.text="Hide"				
		}
		else hideBut.text="Show"
		DataViewController.hideProperties=value
	}
	
	

}