/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import scala.swing._
import scala.swing.event._
import server.config._
import definition.typ._
import server.storage._
import javax.swing.{JOptionPane,BorderFactory,SortOrder,RowSorter,JTable}
import transaction.handling._
import javax.swing.border._
import javax.swing.table._
import scala.collection.JavaConversions._
import server.storage._
import transaction.handling.SessionManager
import java.awt.{SystemTray,TrayIcon,Toolkit}
import java.awt.image.BufferedImage
import java.awt.event.{MouseAdapter,WindowAdapter}
import client.dialog.{LogOutputStream}


//import java.swing.JTable

/**
 * 
 */

trait ClassListListener{
	def classListChanged(classList:Seq[(Int,String)])
}

object MainWindow extends SimpleSwingApplication 
{
	var trayIcon:TrayIcon=_
  var hidden:Boolean=false
  var firstopen=true
	var theModel:DefaultTableModel=null
	var dataList:Array[Array[java.lang.Object]]=Array()
	var shortClassList:Seq[(Int,String)]=Seq.empty
	val usedIDs=collection.mutable.HashSet[Int]()	
	val classListListener = collection.mutable.HashSet[ClassListListener]()	
	var newClass:ServerObjectClass=null
	val consolePanel=new ConsolePanel
	//
	LogOutputStream.registerListener(consolePanel)
	
	
	def dataInit()=
	{					
		generateDataList			
		theModel=new DefaultTableModel(dataList,Array[Object]("ID","Name","Description")) {
			override def getColumnClass(col:Int) = {
				col match {
					case 0 => classOf[java.lang.Integer]
					case _ => classOf[String]
				}
			}
		}		
		val sorter=new TableRowSorter[DefaultTableModel](theModel)
		val sortKeys=List(new RowSorter.SortKey(0,SortOrder.ASCENDING))
		sorter.setSortKeys(sortKeys)
		classTable.peer.setModel(theModel)
		//classTable.peer.setRowSorter(sorter);
		val col= classTable.peer.getColumnModel.getColumn(0)		
		col.setMaxWidth(40)
		col.setResizable(false)
		usedIDs.clear
		usedIDs++= dataList.map(_(0).asInstanceOf[Integer].intValue)
	}
	
	def generateDataList={
		def classListIterator=AllClasses.get.getClassList.valuesIterator
		dataList=(classListIterator.map (	
			x=> Array[Object](new java.lang.Integer(x.id),x.name,x.description))).toSeq.
			sortWith(_(0).asInstanceOf[Integer].intValue<_(0).asInstanceOf[Integer].intValue).toArray
		shortClassList=classListIterator.map ( x=> (x.id,x.name)).toSeq.sortWith(_._2< _._2)
		classListListener.foreach(_.classListChanged(shortClassList))
	}
	
	val classTable= new Table
	{
		override lazy val peer: JTable = new JTable with SuperMixin		
    peer.setAutoResizeMode(Table.AutoResizeMode.LastColumn.id)
    peer.setSelectionMode(Table.IntervalMode.Single.id)		
	}

	val rightPanel = new BorderPanel
	{
		def addIt(c: Component, l: Constraints) 
		{
			super.add(c,l)
		}
		//size=new Dimension(300,500)
	}
	
	override def startup(args: Array[String]) = {
		super.startup(args)					
	}

	

	val managementPanel = new GridPanel(1,2) {
		val belowTopBorder = BorderFactory.createTitledBorder("Management");
		belowTopBorder.setTitlePosition(TitledBorder.BELOW_TOP);
		border=belowTopBorder
		val showTransBut = new Button("Show TransLog")
		val showCachesBut = new Button( "Show Caches")
		val showConsoleBut=new Button("Show Console")
		contents +=   showTransBut+= showCachesBut+=showConsoleBut					 

		listenTo(showTransBut,showCachesBut,showConsoleBut)
		reactions += {					
			case ButtonClicked(`showTransBut`) => showTransData
			case ButtonClicked(`showCachesBut`) => StorageManager.printCacheReport
			case ButtonClicked(`showConsoleBut`) => showConsole
		}				
	}


	val classPanel=new BorderPanel() {
		add (new Label("Class-List"),BorderPanel.Position.North)

		add (new ScrollPane()  
		{
			viewportView= classTable
			preferredSize=new Dimension(280,200)								
		},BorderPanel.Position.Center)      

		add (new GridPanel(1,3)
		{
			val newClassBut =new Button("Create Class")
			val changeClassBut =new Button("Change Class")
			val showDataBut = new Button("Show Data")				

			contents += newClassBut += changeClassBut += showDataBut 					 

			listenTo(newClassBut,changeClassBut,showDataBut)
			reactions += {
				case ButtonClicked(`newClassBut`) => createClass
				case ButtonClicked(`changeClassBut`) => editClassDef
				case ButtonClicked(`showDataBut`) => showData					
			}
		},BorderPanel.Position.South)		
	}


	val mainPanel=	new BorderPanel()  // main panel
	{			
		add ( new BorderPanel() //left rail
		{
			add(classPanel,BorderPanel.Position.Center)				
			add(managementPanel,BorderPanel.Position.South)

		},BorderPanel.Position.West) // left rail

		add ( rightPanel ,BorderPanel.Position.Center) // Center	
		
	} // main Panel
	
	val top:MainFrame = new MainFrame 
	{
		override def closeOperation() 
		{
			SessionManager.shutDown()
			super.closeOperation()
		}
		SessionManager.registerSetupListener(() => {
		  dataInit	
		})
		
		SessionManager.init()
		//SessionManager.init
		title = "Database Management"
		contents = mainPanel
		bounds=new Rectangle(100,200,1100,900)
		peer.addWindowListener(new WindowAdapter()	{
			override def windowClosing(e:java.awt.event.WindowEvent ):Unit= 		{
									if(hidden)removeTray();
				   	
			}
			override def windowIconified(e:java.awt.event.WindowEvent ) 
			{
				//System.out.println("Iconified")
				hideFenster();
			}  
			override def windowOpened(e:java.awt.event.WindowEvent )
			{				
				//System.out.println("opened");
				if(firstopen)
				{
					firstopen=false;
					//hideFenster();
				}
			}		
		});
		initTray
		//java.lang.System.setOut(LogOutputStream.ps)
		
	}	

  def getSelectedType = {
  	val ix:Int= classTable.peer.convertRowIndexToModel( classTable.selection.rows.head)  	
  	dataList(ix)( 0).asInstanceOf[java.lang.Integer].intValue
  }

	def showData():Unit =
	{
		//System.out.println("showData" +classTable.selection.rows +" "+classTable.selection.rows.empty)
		if(! (classTable.selection.rows.isEmpty))		 
		{			
			val typ= getSelectedType 
			//System.out.println("showData "+typ)
			IndexTableModel.setTypeHandler(StorageManager.getHandler(typ))
			rightPanel.addIt(DataViewPanel,BorderPanel.Position.Center)
			rightPanel.peer.invalidate
			rightPanel.peer.revalidate
			rightPanel.repaint
		}
	}
	
	def editClassDef():Unit = {
		if(! (classTable.selection.rows.isEmpty))		 
		{			
			val typ= getSelectedType 
			//System.out.println("editClass "+typ+" "+SessionManager.scl)
			TypeDefPanel.setClass(SessionManager.scl.getClassByID(typ).asInstanceOf[ServerObjectClass],false)			
			rightPanel.addIt(TypeDefPanel,BorderPanel.Position.Center)
			mainPanel.peer.invalidate
			rightPanel.peer.invalidate			
			rightPanel.peer.revalidate
			rightPanel.repaint
		}
	}
	
	def createClass():Unit = {		
			newClass=new ServerObjectClass("",0)			
			TypeDefPanel.setClass(newClass,true)			
			rightPanel.addIt(TypeDefPanel,BorderPanel.Position.Center)
			mainPanel.peer.invalidate
			rightPanel.peer.invalidate			
			rightPanel.peer.revalidate
			rightPanel.repaint		
	}
	

	def showTransData():Unit = {
		rightPanel.addIt(TransLogPanel,BorderPanel.Position.Center)
		rightPanel.peer.invalidate
		rightPanel.peer.revalidate
		
		rightPanel.repaint
	}
	
	def showConsole():Unit = {
		rightPanel.addIt(consolePanel,BorderPanel.Position.Center)
		rightPanel.peer.invalidate
		rightPanel.peer.revalidate
		
		rightPanel.repaint
	}
	
	
	def registerClassListListener(li:ClassListListener) = {
		classListListener+=li
		if(!shortClassList.isEmpty) li.classListChanged(shortClassList)
	}
	
	def updateSeq[T](seq:Seq[T],index:Int,newValue:T) = {
		if(index>=seq.size) seq
		seq.indices.map(i=> if(i==index)newValue else seq(i))
	}
	
	def initTray():Unit= 	{  	 
		if (SystemTray.isSupported())	{         
			// load an image
			val res=getClass.getResource("tray.jpg");		
			if(res==null) {
				java.lang.System.out.println("Kann tray.jpg nicht finden ");
				trayIcon = new TrayIcon(new BufferedImage(30,30,BufferedImage.TYPE_INT_RGB), "Datenbank");				
				return
			}
			else {
				val image = Toolkit.getDefaultToolkit().getImage(res);
				trayIcon = new TrayIcon(image, "new DB");
			}
			trayIcon.setImageAutoSize(true);
			//System.out.println("tray "+trayIcon)
			trayIcon.addMouseListener(new MouseAdapter(){
				override def mouseClicked(e1:java.awt.event.MouseEvent ) {
					//System.out.println("tray click")					
					if (hidden) showFenster()
				}
			});       
			// add the tray image                  
		} 
	}

	def addTray()={
		val tray = SystemTray.getSystemTray();
		try {
			tray.add(trayIcon);

		} catch { 
			case e: Exception => {
				System.err.println(e);
		}	
		}
	}


	def hideFenster() =	{
		//System.out.println("hide")
		top.visible=false		  	
		addTray()
		hidden=true
	}

	def showFenster() =	{
		top.visible=true
		top.peer.setExtendedState(java.awt.Frame.NORMAL);
		removeTray();
		hidden=false;
	}
	
	def removeTray()=	{
		SystemTray.getSystemTray().remove(trayIcon);
	}

}