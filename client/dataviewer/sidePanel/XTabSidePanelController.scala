/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

import scala.swing._
import definition.data.Reference
import definition.typ._
import scala.swing.event.ButtonClicked
import java.awt.{Color,Dimension}
import java.awt.event.{MouseAdapter,MouseEvent}
import client.dataviewer.TypeTableModel
import sun.swing.table.DefaultTableCellHeaderRenderer
import javax.swing.BorderFactory
import definition.expression.Expression
import client.dataviewer.MultilineEditor
import javax.swing.table.TableCellEditor

/** Controller for XTab SidePanel
 * 
 */
class XTabSidePanelController extends SidePanelController {
	
	val tmodel=new XTabSidePanelModel(this)
	var ydataModel:TypeTableModel= _
	
	var container:ControllerContainer=_
	
	var dropTransferHandler=new XTabHeaderTransferHandler(XTabSidePanelController.this)
	
	val headerDropComp=new Panel {
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		//preferredSize=new Dimension(10,50)
		//background=Color.blue
		border=BorderFactory.createEtchedBorder()
		peer.setTransferHandler(dropTransferHandler)
	}
	
	val rightDropComp=new Panel {
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		//preferredSize=new Dimension(10,0)
		yLayoutAlignment=0d
		//background=Color.cyan
		//opaque=true
		border=BorderFactory.createEtchedBorder()
		peer.setTransferHandler(dropTransferHandler)
	}
	
	val table=new Table{
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None  
		peer.setAutoCreateColumnsFromModel(false)
		model=tmodel
		showGrid=true
		gridColor=Color.gray
		//println("UI:" +peer .getTableHeader.getUI)
		peer.setColumnModel(tmodel.colModel .colModel )
		
		override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int):Component = {
			//FIND VALUE
			tmodel.structure match {
				case Some(s)=> {
					val modCol=peer.convertColumnIndexToModel(col)
					val rendCol=s.getActualColumnModelIndex(modCol % s.numDataCellFields )+1
					val v=model.getValueAt(row,modCol)
					//if(row==0 && col==1)println("rc:"+rendCol+" v:"+v.asInstanceOf[Expression].getTerm)
					v match {
						case ve:Expression => return s.itcr.componentFor(this,sel,foc,ve,row,rendCol)
						case vt:Tuple2[_,_] => return s.etcr.componentFor(this,sel,foc,v.asInstanceOf[Tuple2[String,Int]],row,rendCol)
						case _=> 
					}	
				}
				
				case None =>
			}
			super.rendererComponent(sel,foc,row,col)						 		 
		}
		
		override def editor(row: Int, column: Int):TableCellEditor={instEditor	}
	}
	
	val instEditor:MultilineEditor=new MultilineEditor(table.peer)	{
		def setEditorValue(value:Object) = {
			if (value.isInstanceOf[Expression]){
				val expr=value.asInstanceOf[Expression]
				if(expr.getType==DataType.StringTyp) expr.toString
				else expr.getTerm
			}
			else if(value==null)"" else value.toString
		}
	}
	
	lazy val XTabRowType:Int=SystemSettings().systemTypes("XTabRow")
	
	def getDefaultHeaderRenderer=table.peer.getTableHeader.getDefaultRenderer
	
	def classFits(tableClass:AbstractObjectClass):Boolean = {
		//println("XTabRowType:"+XTabRowType)
		//println("Class Fits:"+tableClass.inheritsFrom(XTabRowType))
		tableClass.inheritsFrom(XTabRowType)
	}
	
	private def setYDataModel(ymod:TypeTableModel) = {
		ydataModel=ymod
	  table.rowHeight=ymod.defaultRowHeight
	  table.font=ymod.tableFont
	}
	
  def parentsFits(dataModel:TypeTableModel,parentRef:Reference):Boolean = 
  {
  	setYDataModel(dataModel)
  	AllClasses.get.getClassByID(parentRef.typ).inheritsFrom(XTabRowType)  	
  }
	

  def panelName:String="XT"

  def openPanel(parentRef: Reference, tableClass:AbstractObjectClass,cont:ControllerContainer): Unit = { 
  	//println("openPanel:"+parentRef)
  	container=cont
  	tmodel.initData(parentRef)  	
  }
  
  def closePanel:Unit = {
  	tmodel.shutDown
  }

  

  lazy val headerComp= new BoxPanel(Orientation.Horizontal ) {
  	val closeBut=new Button (" < ")
  	closeBut.yLayoutAlignment=1d
  	headerDropComp.yLayoutAlignment=1d
  	val header=table.peer.getTableHeader
  	val headerWrapper= new XTabHeaderPanel(header)
  	headerWrapper.peer.setMaximumSize(new Dimension(tmodel.colModel.columnWidth,headerWrapper.preferredSize.height))
  	//border=BorderFactory.createLineBorder(Color.red,0)
  	contents+=headerWrapper  	
  	contents+=headerDropComp
  	contents+=closeBut
  	listenTo(closeBut)
  	reactions += {
  		case b: ButtonClicked => {
  			container.closeSideBar
  		}
  	}
  	table.peer.getTableHeader.addMouseListener(new MouseAdapter(){
		override def mouseClicked(ev:MouseEvent) = {
			var x=ev.getPoint.x
			val colMod=tmodel.colModel.colModel
			val col=colMod.getColumnIndexAtX(x)
			var offset=0
			for(i <-0 to col) offset+=colMod.getColumn(i).getWidth
			val pos=offset-x
			if(pos<23 && pos>0){
				//println("hit "+col)
				val modCol=table.peer.convertColumnIndexToModel(col)
				tmodel.deleteColumn(modCol)
			}			
		}
	})
  }

  lazy val mainComp= new BoxPanel(Orientation.Horizontal ) {
  	contents+=new XTabMainPanel(table)  	
  	contents+=rightDropComp
  }
  
  def notifyRowsChanged:Unit = {
  	tmodel.notifyRowsChanged
  	mainComp.peer.invalidate
  }

}
