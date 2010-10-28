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
import client.dataviewer._
import client.dialog._
import client.graphicsView._

/** tests the instance dataviewer
 * 
 */
object ViewTest extends SimpleSwingApplication {
	var sock:ClientSocket=null	
	
	
	val typEdit=new TextField("10")
	val instEdit=new TextField("1")
	val propEdit=new TextField("0")	
	
	val viewController=new DataViewController()
	
	val pathMod=new PathModel()
	val pathView=new ListView[InstanceData]()
	val pathContr=new PathController(pathMod,pathView,viewController)
	
	val actionPan= new ActionPanel
	
	val fieldEditPan=new FieldEditorsPanel
	val newPanelArea=new NewPanelArea
	
	val testGraphList=new ListView[GraphElem]()
	
	//var lastSelected:Seq[Referencable]=Seq.empty
	
	val graphViewController=new GraphViewController
	val layerPanController=new LayerPanelController(graphViewController)
	
	val graphViewPan=new BorderPanel () {
		preferredSize=new Dimension(400,200)
		add(new ScrollPane() {
			preferredSize=new Dimension(200,200)
			viewportView=graphViewController.canvasPanel
			testGraphList.peer.setModel(TestGraphListModel)
		},BorderPanel.Position.Center)
		add(layerPanController.layerPanel,BorderPanel.Position.North)
		viewController.registerSelectListener(layerPanController)
	}
	
	val pathScroller =new ScrollPane() {
				viewportView= pathView
				pathView.fixedCellHeight=30
				preferredSize=new Dimension(200,200)
				def callback(nsize:Int):Unit= {
					//println(size+" "+pathView.peer.getFixedCellHeight())
					preferredSize=new Dimension(400,(nsize+1)*30 )
					maximumSize=preferredSize
					mainPanel.revalidate
					peer.invalidate
				}
				pathContr.registerSizeChangeListener(callback)
			}
	
	val mainPanel:BorderPanel=	new BorderPanel()  // main panel
	{			
		add ( new GridPanel(1,8) //top rail
		{
			hGap=10
			border=BorderFactory.createEmptyBorder(10,10,10,10);
			val loadBut =new Button("load")
			val openGraphBut = new Button("open Graph")
			//val deleteBut =new Button("delete Instance")
			//val stressBut = new Button("stress test")
			val copyBut = new Button("copy")
			val createBut= new Button("create")
			contents += typEdit += instEdit += propEdit += loadBut +=  copyBut+=createBut +=openGraphBut
			listenTo(loadBut,copyBut,createBut,openGraphBut)
			reactions += {
					case ButtonClicked(`loadBut`) => loadData
					//case ButtonClicked(`openGraphBut`) => openGraphData
					//case ButtonClicked(`deleteBut`) => deleteInstance
					//case ButtonClicked(`stressBut`) => stressTest
					case ButtonClicked(`copyBut`) => copyData
					case ButtonClicked(`createBut`) => createInstance
			}
		},BorderPanel.Position.North)
		
		add (graphViewPan,BorderPanel.Position.Center) 
		
		add (new BorderPanel(){
			add (pathScroller,BorderPanel.Position.North)
			add ( viewController.panel,BorderPanel.Position.Center)
			/*viewController.registerSelectListener(new SelectListener{
					def selectionChanged(sender:SelectSender,instList:Seq[Referencable])= lastSelected=instList
				})*/
		},BorderPanel.Position.West)
		
		add( new BorderPanel() {
			peer.putClientProperty("newPanel","IGNORE")
			add(new BoxPanel(scala.swing.Orientation.Vertical){
				contents+=newPanelArea
				contents+=fieldEditPan
				
			},BorderPanel.Position.North)			
			add(new BorderPanel() {
				preferredSize=new Dimension(120,100)
				add(new Label("Funktionen:"){preferredSize=new Dimension(40,35)},BorderPanel.Position.North)
				add (new ScrollPane() {
					viewportView= actionPan				
				},BorderPanel.Position.Center)
			},BorderPanel.Position.Center)
		},BorderPanel.Position.East)
		add( DialogManager.dialogPanel,BorderPanel.Position.South)
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
		bounds=new Rectangle(200,200,1600,900)
	}	
	
	
	
	override def startup(args: Array[String]) = {		
		if(args.length<4 ) { println("Usage: TableTest host portnr name password"); quit() }
		top.title= "Table Test ["+args(2)+"]"
		// connect components
		actionPan.registerActionPanListener(DialogManager)
		viewController.registerSelectListener(actionPan)
		viewController.registerSelectListener(DialogManager)
		viewController.registerSelectListener(fieldEditPan)
		graphViewController.selectModel.registerSelectListener(actionPan)
		graphViewController.selectModel.registerSelectListener(DialogManager)
		graphViewController.selectModel.registerSelectListener(fieldEditPan)
		DialogManager.answerArea.registerCustomPanel[PointAnswerPanel](DataType.VectorTyp)
		
		//println(top.title)
		sock=new ClientSocket(InetAddress.getByName(args(0)),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()
  	
  	super.startup(args)
	}
	
	
	
	def loadData() = {
		val newRef=Reference(typEdit.text.toInt,instEdit.text.toInt)
		//tabModel.loadData(newRef,propEdit.text.toByte)
		pathContr.loadPath( IndexedSeq(newRef))
	}
	
	def createInstance() = {
		ClientQueryManager.createInstance(typEdit.text.toInt,Array(new OwnerReference(propEdit.text.toByte,viewController.parentRef)))
	}
	 
	
		
	def deleteInstance():Unit = {
		if(viewController.selectedInstance ==null) return		
		val r=ClientQueryManager.deleteInstance(viewController.selectedInstance.ref)
		println("Delete:"+r  )
	}
	
	def stressTest() = {
		val numLoops=100
		val starttime:Long = System.currentTimeMillis();
		val owner=Array(new OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt)))		
		
		
		val endtime = System.currentTimeMillis()
		println("Stresstest time:"+(endtime-starttime))
	}
	
	def copyData():Unit = {
		val fromOwner=OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt))
		val toOwnerList:Array[String]= JOptionPane.showInputDialog(null, "to owner: typ,instance,field ").split(",");
		val toOwner=OwnerReference(toOwnerList(2).toByte,Reference(toOwnerList(0).toInt,toOwnerList(1).toLong))
		if(viewController.selectedInstance==null) return		
		val starttime:Long = System.currentTimeMillis();
		val inst=ClientQueryManager.copyInstance(viewController.selectedInstance.ref,fromOwner,toOwner)	
		val endtime = System.currentTimeMillis()
		println("copy time:"+(endtime-starttime))
		println("inst:"+inst)
	}
	
	def shutDown() = {
		println("shutdown")
		sock.quitApplication()
	}

}