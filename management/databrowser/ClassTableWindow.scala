/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import scala.swing._
import scala.swing.event._
import server.config._
import definition.typ._
import server.storage._
import javax.swing.JOptionPane
import transaction.handling._

//import java.swing.JTable

/**
 * 
 */
object ClassTableWindow extends SimpleSwingApplication 
{
	def dataInit()=
	{		
		SessionManager.init()
		TypTableModel.setClassList(AllClasses.getClassList.valuesIterator.toArray)		
	}

	val classTable= new Table
	{
		model=TypTableModel
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
	}

	val rightPanel = new BorderPanel
	{
		def addIt(c: Component, l: Constraints) 
		{
			super.add(c,l)
		}
		//size=new Dimension(300,500)
	}

	def top = new MainFrame 
	{

		override def closeOperation() 
		{
			StorageManager.shutDown()
			super.closeOperation()
		}


		dataInit
		title = "Database Management"

		contents = mainPanel
		bounds=new Rectangle(200,200,800,600)
	}	
	
	
	val mainPanel=	new BorderPanel()  // main panel
	{			
		add ( new BorderPanel() //left rail
		{
			add (new Label("Class-List"),BorderPanel.Position.North)

			add (new ScrollPane()  
			{
				viewportView= classTable
				preferredSize=new Dimension(280,200)								
			},BorderPanel.Position.Center)      
			
		  add (new GridPanel(6,1)
			{
				val newClassBut =new Button("Create Class")
				val newVersionBut =new Button("Create new Version")
				val showDataBut = new Button("Show Data")
				val showIndexBut = new Button("Show Index")
				val showTransBut = new Button("Show Trans Log")
				val showCachesBut = new Button( "Show Caches")
				contents += showDataBut += newClassBut += newVersionBut += showIndexBut += showTransBut+= showCachesBut					 

				listenTo(newClassBut,newVersionBut,showDataBut,showIndexBut,showTransBut,showCachesBut)
				reactions += {
					case ButtonClicked(`newClassBut`) => JOptionPane.showMessageDialog(null, "Create Class")
					case ButtonClicked(`newVersionBut`) => JOptionPane.showMessageDialog(null, "Create Version")
					case ButtonClicked(`showDataBut`) => showData
					case ButtonClicked(`showTransBut`) => showTransData
					case ButtonClicked(`showCachesBut`) => StorageManager.printCacheReport
				}
			},BorderPanel.Position.South)				


		},BorderPanel.Position.West) // left rail

		add ( rightPanel ,BorderPanel.Position.Center) // Center
		
		classTable.peer.getColumnModel.getColumn(0).setMaxWidth(25) 
		classTable.peer.getColumnModel.getColumn(0).setPreferredWidth(25)
		classTable.peer.getColumnModel.getColumn(0).setWidth(25)
	} // main Panel
			


	def showData():Unit =
	{
		//println("showData" +classTable.selection.rows +" "+classTable.selection.rows.empty)
		if(! (classTable.selection.rows.isEmpty))		 
		{			
			val ix:Int= classTable.selection.rows.head
			val typ= TypTableModel.classList(ix).id
			//println("showData "+ix+" "+typ)
			IndexTableModel.setTypeHandler(StorageManager.getHandler(typ))
			rightPanel.addIt(DataViewPanel,BorderPanel.Position.Center)
			rightPanel.peer.invalidate
			rightPanel.peer.revalidate
			rightPanel.repaint
		}
	}
	
	def showTransData():Unit = {
		rightPanel.addIt(TransLogPanel,BorderPanel.Position.Center)
		rightPanel.peer.invalidate
		rightPanel.peer.revalidate
		rightPanel.repaint
	}

}