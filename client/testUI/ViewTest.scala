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
	
	
	//val typEdit=new TextField("10")
	//val instEdit=new TextField("1")
	//val propEdit=new TextField("0")	
	
	//val viewController=new DataViewController()
	
	//val pathMod=new PathModel()
	//val pathView=new ListView[InstanceData]()
//	val pathContr=new PathController(pathMod,pathView,viewController)
	
	val actionPan= new ActionPanel
	
	val fieldEditPan=new FieldEditorsPanel
	//val newPanelArea=new NewPanelArea
	
	val undoBut = new Button("Undo")
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
	
	//val testGraphList=new ListView[GraphElem]()
	
	//var lastSelected:Seq[Referencable]=Seq.empty
	
	
	
	
	
	
	
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
				preferredSize=new Dimension(120,100)
				add(new Label("Funktionen:"){preferredSize=new Dimension(40,30)},BorderPanel.Position.North)
				add (new ScrollPane() {
					viewportView= actionPan				
				},BorderPanel.Position.Center)
			},BorderPanel.Position.Center)
		},BorderPanel.Position.West)
		add( new BoxPanel(Orientation.Horizontal) {
			contents+= undoBut+=Swing.HStrut(50)+=DialogManager.dialogPanel
		},BorderPanel.Position.South)
		
		listenTo(undoBut)
		reactions += {					
					case ButtonClicked(`undoBut`) => requestUndoData					
		}
	}
	
	val top = new MainFrame ()
	{
		override def closeOperation() 
		{
			println("Close " )			
			shutDown()
			super.closeOperation()			
			
		}
		title="TableTest"
		contents = mainPanel
		bounds=new Rectangle(20,20,1500,900)
	}	
	
	
	
	override def startup(args: Array[String]) = {		
		if(args.length<4 ) { println("Usage: TableTest host portnr name password"); quit() }		
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
		//println(top.title)
		sock=new ClientSocket(InetAddress.getByName(args(0)),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()
  	
  	super.startup(args)
	}
	
	
	
	
	
	/*def createInstance() = {
		ClientQueryManager.createInstance(typEdit.text.toInt,Array(new OwnerReference(propEdit.text.toByte,viewController.ref)))
	}*/
	 
	
		
	/*def deleteInstance():Unit = {
		if(viewController.selectedInstance ==null) return		
		val r=ClientQueryManager.deleteInstance(viewController.selectedInstance.ref)
		println("Delete:"+r  )
	}*/
	
	/*def stressTest() = {
		val numLoops=100
		val starttime:Long = System.currentTimeMillis();
		val owner=Array(new OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt)))		
		
		
		val endtime = System.currentTimeMillis()
		println("Stresstest time:"+(endtime-starttime))
	}
	
	def copyData():Unit = {
		val fromOwner=OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt))
		val toOwnerList:Array[String]= JOptionPane.showInputDialog(null, "to owner: typ,instance,field ").split(",");
		val toOwner=OwnerReference(toOwnerList(2).toByte,Reference(toOwnerList(0).toInt,toOwnerList(1).toInt))
		if(viewController.selectedInstance==null) return		
		val starttime:Long = System.currentTimeMillis();
		val inst=ClientQueryManager.copyInstance(viewController.selectedInstance.ref,fromOwner,toOwner)	
		val endtime = System.currentTimeMillis()
		println("copy time:"+(endtime-starttime))
		println("inst:"+inst)
	}*/
	
	def shutDown() = {
		println("shutdown")
		sock.quitApplication()
	}
	
	def requestUndoData = {
		undoDialog.setLocationRelativeTo(undoBut)
		undoDialog.size=new Dimension(600,300)
		
		//undoDialog.visible=true		
		ClientQueryManager.requestUndoData()
	}

}