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

/**
 * 
 */
object TableTest extends SimpleSwingApplication {
	var sock:ClientSocket=null
	val tabModel=new SimpleTableModel()
	
	def top = new MainFrame 
	{
		override def closeOperation() 
		{
			println("Close " )			
			shutDown()
			super.closeOperation()			
			
		}		
		title = "Table Test"
		contents = mainPanel
		bounds=new Rectangle(200,200,800,600)
	}	
	
	val typEdit=new TextField("3")
	val instEdit=new TextField("1")
	val propEdit=new TextField("0")
	
	val dataTable= new Table
	{
		model=tabModel
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
	}
	
	val mainPanel=	new BorderPanel()  // main panel
	{			
		add ( new GridPanel(1,8) //top rail
		{
			hGap=10
			border=BorderFactory.createEmptyBorder(10,10,10,10);
			val loadBut =new Button("load")
			val openBut = new Button("open")
			val deleteBut =new Button("delete Instance")
			val stressBut = new Button("stress test")
			val copyBut = new Button("copy")
			contents += typEdit += instEdit += propEdit += loadBut += openBut += deleteBut += stressBut+=copyBut
			listenTo(loadBut,deleteBut,stressBut,copyBut,openBut)
			reactions += {
					case ButtonClicked(`loadBut`) => loadData
					case ButtonClicked(`openBut`) => openData
					case ButtonClicked(`loadBut`) => deleteInstance
					case ButtonClicked(`stressBut`) => stressTest
					case ButtonClicked(`copyBut`) => copyData
			}
		},BorderPanel.Position.North)
		add (new ScrollPane()  
			{
				viewportView= dataTable
				preferredSize=new Dimension(280,200)								
			},BorderPanel.Position.Center) 
	}
	
	
	
	override def startup(args: Array[String]) = {		
		if(args.length<4 ) { println("Usage: TableTest host portnr name password"); quit() }
		sock=new ClientSocket(InetAddress.getByName(args(0)),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()
  	super.startup(args)
	}
	
	def loadData() = {
		tabModel.loadData(Reference(typEdit.text.toInt,instEdit.text.toInt),propEdit.text.toByte)
	}
	
	def openData() = {
		val row:Int=(dataTable.selection.rows.toArray).apply(0)
		tabModel.loadData(tabModel.getInstance(row).ref,propEdit.text.toByte)
	}
	
	def deleteInstance() = {
		
	}
	
	def stressTest() = {
		val numLoops=100
		val starttime:Long = System.currentTimeMillis();
		val owner=Array(new OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt)))
		
		if(tabModel.dataList !=null)
		for(i <- 0 until 500)
		{				
				val id=ClientQueryManager.createInstance(tabModel.allowedClass,owner)
  			ClientQueryManager.writeInstanceField(Reference(tabModel.allowedClass,id),0,StringConstant("Test Object "+i))
  			ClientQueryManager.writeInstanceField(Reference(tabModel.allowedClass,id),1,DoubleConstant(i.toDouble/100))
		}
		val endtime = System.currentTimeMillis()
		println("Stresstest time:"+(endtime-starttime))
	}
	
	def copyData():Unit = {
		val fromOwner=OwnerReference(propEdit.text.toByte,Reference(typEdit.text.toInt,instEdit.text.toInt))
		val toOwnerList:Array[String]= JOptionPane.showInputDialog(null, "to owner: typ,instance,field ").split(",");
		val toOwner=OwnerReference(toOwnerList(2).toByte,Reference(toOwnerList(0).toInt,toOwnerList(1).toLong))
		if(dataTable.selection.rows.isEmpty) return
		val row:Int=(dataTable.selection.rows.toArray).apply(0)
		val starttime:Long = System.currentTimeMillis();
		val inst=ClientQueryManager.copyInstance(tabModel.getInstance(row).ref,fromOwner,toOwner)	
		val endtime = System.currentTimeMillis()
		println("copy time:"+(endtime-starttime))
		println("inst:"+inst)
	}
	
	def shutDown() = {
		println("shutdown")
		sock.quitApplication()
	}

}