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
import definition.expression._
import client.model._
import client.comm._
import java.net._
import java.io._
import javax.swing.border._
import scala.collection.immutable.IndexedSeq
import client.dataviewer._
import client.dialog._

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
	
	val mainPanel=	new BorderPanel()  // main panel
	{			
		add ( new GridPanel(1,8) //top rail
		{
			hGap=10
			border=BorderFactory.createEmptyBorder(10,10,10,10);
			val loadBut =new Button("load")
			//val openBut = new Button("open")
			val deleteBut =new Button("delete Instance")
			val stressBut = new Button("stress test")
			val copyBut = new Button("copy")
			val createBut= new Button("create")
			contents += typEdit += instEdit += propEdit += loadBut +=  deleteBut += stressBut+=copyBut+=createBut
			listenTo(loadBut,deleteBut,stressBut,copyBut,createBut)
			reactions += {
					case ButtonClicked(`loadBut`) => loadData
					//case ButtonClicked(`openBut`) => openData
					case ButtonClicked(`deleteBut`) => deleteInstance
					case ButtonClicked(`stressBut`) => stressTest
					case ButtonClicked(`copyBut`) => copyData
					case ButtonClicked(`createBut`) => createInstance
			}
		},BorderPanel.Position.North)
		add (new ScrollPane()  
			{
				viewportView= viewController.panel
				//preferredSize=new Dimension(280,500)								
			},BorderPanel.Position.Center) 
		add (new BorderPanel(){
			add (new ScrollPane() {
				viewportView= pathView
				preferredSize=new Dimension(200,200)
			},BorderPanel.Position.Center)
		},BorderPanel.Position.West
		)
		add( new BorderPanel() {
			preferredSize=new Dimension(120,100)
			add(new Label("Funktionen:"),BorderPanel.Position.North)
			add (new ScrollPane() {
				viewportView= actionPan				
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
		bounds=new Rectangle(200,200,900,600)
	}	
	
	
	
	override def startup(args: Array[String]) = {		
		if(args.length<4 ) { println("Usage: TableTest host portnr name password"); quit() }
		top.title= "Table Test ["+args(2)+"]"
		// connect components
		actionPan.registerActionPanListener(DialogManager)
		viewController.registerSelectListener(actionPan)
		viewController.registerSelectListener(DialogManager)
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
		
		
		for(i <- 0 until 500)
		{				
				/*val id=ClientQueryManager.createInstance(tabModel.allowedClass,owner)
  			ClientQueryManager.writeInstanceField(Reference(tabModel.allowedClass,id),0,StringConstant("Test Object "+i))
  			ClientQueryManager.writeInstanceField(Reference(tabModel.allowedClass,id),1,DoubleConstant(i.toDouble/100))*/
		}
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