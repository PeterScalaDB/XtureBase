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
	
	val mainPanel=	new BorderPanel()  // main panel
	{			
		add ( new GridPanel(1,5) //top rail
		{
			hGap=20
			border=BorderFactory.createEmptyBorder(10,10,10,10);
			val loadBut =new Button("load")
			val deleteBut =new Button("delete Instance")
			contents += typEdit += instEdit += propEdit += loadBut += deleteBut
			listenTo(loadBut,deleteBut)
			reactions += {
					case ButtonClicked(`loadBut`) => loadData
					case ButtonClicked(`loadBut`) => deleteInstance
			}
		},BorderPanel.Position.North)
		add (new ScrollPane()  
			{
				viewportView= dataTable
				preferredSize=new Dimension(280,200)								
			},BorderPanel.Position.Center) 
	}
	
	def dataTable= new Table
	{
		model=tabModel
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
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
	
	def deleteInstance() = {
		
	}
	
	def shutDown() = {
		println("shutdown")
		sock.quitApplication()
	}

}