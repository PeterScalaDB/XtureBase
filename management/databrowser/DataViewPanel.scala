/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import scala.swing._
import scala.swing.event._
import javax.swing.JOptionPane
import server.storage._
import transaction.handling._
import definition.data._

/** Panel to show the Data of a certain class
 * 
 */
object DataViewPanel extends BorderPanel 
{
	val ixTable = new Table()
	{
		model=IndexTableModel				
		selection.intervalMode=Table.IntervalMode.Single
	}	
	
	val fieldTable= new Table()
	{
		model=InstFieldTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}
	
	val propTable=new Table()
	{
		model=InstPropTableModel
		selection.intervalMode=Table.IntervalMode.Single
	}

	add (new BorderPanel() // left rail
	{
		preferredSize=new Dimension(150,200)
		add (new Label("Index-Table"),BorderPanel.Position.North)
		add (new ScrollPane()
		{
			viewportView=ixTable 
		},BorderPanel.Position.Center)

		add (new GridPanel(4,1)
		{
			val openBut=new Button("Open Instance")
			val createBut=new Button("Create Instance")
			val deleteBut=new Button("Delete Instance")
			val checkCollBut=new Button("check CollData")
			
			contents+=openBut+=createBut+=deleteBut+=checkCollBut
			listenTo(openBut,createBut,deleteBut,checkCollBut)
			reactions += 
			{
				case ButtonClicked(`openBut`) => openInstance
				case ButtonClicked(`createBut`) => createInstance
				case ButtonClicked(`deleteBut`) => deleteInstance
				case ButtonClicked(`checkCollBut`) => checkCollData
			}
		},BorderPanel.Position.South)

	},BorderPanel.Position.West) // left rail

	add (new BorderPanel() // center rail
	{
		add (new BorderPanel()
		{
		  add (new Label("Instance view"),BorderPanel.Position.North)
		  add (new ScrollPane()
		  {
		  	viewportView= fieldTable
		  	preferredSize=new Dimension(200,300)
		  },BorderPanel.Position.Center)
		  
		},BorderPanel.Position.Center)
		
		add (new BorderPanel(){
			preferredSize=new Dimension(200,200)
			add (new Label("Property Data"),BorderPanel.Position.North)
			add (new ScrollPane(){
				viewportView=propTable
				preferredSize=new Dimension(200,150)
			},BorderPanel.Position.Center)
		},BorderPanel.Position.South)
		
	},BorderPanel.Position.Center)

	
	def openInstance() =
	{		
		//println("openBut "+ixTable.selection.rows)
		if(! (ixTable.selection.rows.isEmpty))		 
		{			
			val ix:Int= ixTable.selection.rows.head
			val inst:Long=IndexTableModel.ixList(ix)._1
			//println("inst: "+inst)
			if (StorageManager.instanceExists(InstFieldTableModel.theClass.id,inst))
			{
				val r=new Reference(InstFieldTableModel.theClass.id,inst)
				val i=StorageManager.getInstanceData(r)
		    InstFieldTableModel.setInstance(i)
		    InstPropTableModel.setPropData(StorageManager.getInstanceProperties(r),i.classVersion)
			} 
		}
	}
	
	def createInstance() =
	{
		TransactionManager.doTransaction{
		  val inst=TransactionManager.tryCreateInstance(InstFieldTableModel.theClass.id, Array())
		  //TransactionManager.tryWriteInstanceData(inst)	
		}		
		IndexTableModel.readTheList
	}
	
	def deleteInstance() =
	{
		if(! (ixTable.selection.rows.isEmpty))		 
		{			
			val ix:Int= ixTable.selection.rows.head
			val inst:Long=IndexTableModel.ixList(ix)._1
			//println("inst: "+inst)
		  InstFieldTableModel.setInstance(null)	
		  TransactionManager.doTransaction{
			  TransactionManager.tryDeleteInstance(new Reference(InstFieldTableModel.theClass.id,inst),None)	
			}		  
		  
		  IndexTableModel.readTheList
		}
	}
	
	def checkCollData() =
	{
		if(! (ixTable.selection.rows.isEmpty))		 
		{			
			val ix:Int= ixTable.selection.rows.head
			val inst:Long=IndexTableModel.ixList(ix)._1
			//println("inst: "+inst)
			
			println(ActionList.getCollData(new Reference(InstFieldTableModel.theClass.id,inst)))		  
		  
		}
	}
}