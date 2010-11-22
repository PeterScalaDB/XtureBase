/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer
import client.comm._
import collection.generic.{GenericCompanion}
import definition.data._
import definition.expression._

import definition.typ._
import javax.swing._
import javax.swing.event._
import javax.swing.table._
import java.awt.{Color, Dimension, Font}
import java.awt.event.MouseAdapter
import scala.swing._
import scala.swing.event._

/** table model for a table showing instances of a certain type
 * 
 */
class TypeTableModel(val typ:Int, propMod:PropertyModel) extends AbstractTableModel {
	val objClass=AllClasses.get.getClassByID(typ)
	var dataList:Seq[InstanceData]= _
	val tableFont=new Font("Arial",0,15)
	var selfSelectChanged=false
	var selfAdded=false
	val selectedInstances=new SelectList(dataList)
	var dragColumn:Int= -1
	var dragColumnNewPos:Int=0
	
	val defaultRowHeight=20
	
	val transferHandler=new TableTransferHandler(this)

	val listLock=new Object

	val table:Table=new Table(){		
		autoResizeMode=Table.AutoResizeMode.Off
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row 
		peer.setAutoCreateColumnsFromModel(false)
		//peer.setFillsViewportHeight(true)
		peer.setTransferHandler(transferHandler)
		peer.setDragEnabled(true)
		peer.setDropMode(DropMode.ON_OR_INSERT_ROWS)
		rowHeight=defaultRowHeight
		font=tableFont
		
		listenTo(selection)	
		listenTo(mouse.clicks,this)
		reactions += {
			case TableRowsSelected(table,range,live) => {			
				if (!live&& ! selfSelectChanged) listLock.synchronized  { 
					//println("range:"+range+" rows:"+selection.rows+" DataList:"+(if(dataList==null) "null" else dataList.size)
					//	+ " buf:"+(if(selectedInstances.buf!=null)selectedInstances.buf.size else "null"))
					selectedInstances.setFilter(peer.getSelectedRows)										
					propMod.mainController.selectionChanged(TypeTableModel.this,propMod,selectedInstances)
				}
			}				
			case e: MouseClicked => 
			if(dataList!=null && peer.columnAtPoint(e.point)==0 && e.clicks==1 
					&& e.triggersPopup == false &&  (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1) )	{
				val row= peer.rowAtPoint(e.point)
				if(row>=0 && row<dataList.size) listLock.synchronized { propMod.mainController.openChild(dataList(row).ref)}
			}
			case e:FocusGained =>propMod.focusGained
		}
		model=TypeTableModel.this
		peer.setColumnModel(TableHeaderMap.getColumnModel(typ))		
		showGrid=true
		gridColor=Color.gray
		// prevent first column from getting moved
		peer.getColumnModel().addColumnModelListener(new TableColumnModelListener(){ 
			def columnAdded(e:TableColumnModelEvent ) {}
			def columnMarginChanged(e:ChangeEvent ) {} 
			def columnMoved(e:TableColumnModelEvent){ 
				if (dragColumn == -1) 
					dragColumn = e.getFromIndex();
				dragColumnNewPos = e.getToIndex(); 
			}
			def columnRemoved(e:TableColumnModelEvent ) {}
			def columnSelectionChanged(e:javax.swing.event.ListSelectionEvent ) {} 
		});
		peer.getTableHeader().addMouseListener(new java.awt.event.MouseAdapter() { 
			override def mouseReleased(e:java.awt.event.MouseEvent) {
				if (dragColumn != -1 && (dragColumn == 0 || dragColumnNewPos == 0)) 
					peer.moveColumn(dragColumnNewPos, dragColumn); 
				dragColumn = -1;  dragColumnNewPos = -1; 
			} 
		}); 
	}	
	
	def getParentRef=propMod.mainController.ref
	def getPropField= propMod.propertyField

	val scroller:ScrollPane=new ScrollPane() {
		
		viewportView=table
		preferredSize=new Dimension(100,100)		
	}

	def setDataList(data:Seq[InstanceData],selectInstance:Option[Reference]) =  {
		listLock.synchronized {
			//println("tableMod set Data "+data.mkString)
			dataList=data			
			selfSelectChanged=false
			selfAdded=false
			selectedInstances.buf=data
			selectedInstances.setFilter(Array())
		}		 
			fireTableStructureChanged()
			calcSize()
			selectInstance match {
				case Some(ref) =>if(ref.typ == typ){
					val ix= dataList.findIndexOf(_.ref==ref)
					if(ix>=0)  table.selection.rows+=ix				
				}
				case _ =>
			}
		//setupColumns()		
	}
	
	
	
	def calcSize() = {
		
		val tabPrefHeight=table.preferredSize.height+defaultRowHeight*2+6
		val mainPanelHeight=propMod.mainController.panel.size.height
	  if(tabPrefHeight>(mainPanelHeight-defaultRowHeight)) 
	  	println("tabprefHeight:"+tabPrefHeight+" > mainpanelHeight"+mainPanelHeight)
		scroller.preferredSize=new Dimension(100,if(tabPrefHeight>(mainPanelHeight-defaultRowHeight)&&mainPanelHeight>defaultRowHeight*2)
			(mainPanelHeight-defaultRowHeight) else tabPrefHeight )
		//scroller.preferredSize=new Dimension(100,(if (dataList==null || dataList.isEmpty)2 else dataList.size+2)*22)
		scroller.maximumSize=new Dimension(2000,scroller.preferredSize.height)
		propMod.mainController.updateHeight()
		//println("typTable calcsize "+scroller.preferredSize+ " tabPrefHeight "+tabPrefHeight+" mainpan: "+mainPanelHeight)
		//scroller.revalidate
	}

	def changeInstance(newInst:InstanceData):Unit = listLock.synchronized {
		//println("tablemod change inst: "+newInst.ref)
		val pos = { var ret= -1
			for(i <-0 until dataList.size;if(dataList(i).ref==newInst.ref)) {ret=i} 
			ret
		}
		//println("change "+newInst.ref+ " size:"+dataList.size+ " pos:"+pos+ " list:"+dataList.mkString+ "  "+ Thread.currentThread)
		if(pos<0) println("prop "+propMod.propertyField+" Table typ: "+typ+" Change Instance "+newInst.ref+" not found ! " + dataList.size+" "+Thread.currentThread)
		else  { 
			dataList=dataList.updated(pos,newInst)
			selectedInstances.buf=dataList
			/*propMod.runSw*/fireTableRowsUpdated(pos,pos)
				
		}	
	}

	def addInstance(newInst:InstanceData) = listLock.synchronized {
		//println("tablemod add inst: "+newInst.ref)
		if(dataList==null) {
			dataList=IndexedSeq(newInst)
			//setupColumns()
		}
		else {
			//if(pos<0 || pos >=dataList.size)
				dataList=dataList :+ newInst
				//else  dataList=(dataList.take(pos):+newInst)++ dataList.drop(pos)
		}
		selectedInstances.buf=dataList
		//println("added "+dataList.size+" "+Thread.currentThread+ " "+table.selection.rows)
		val newSize=dataList.size
		
		/*propMod.runSw*/{fireTableRowsInserted(newSize,newSize)
			calcSize()
			if(selfAdded){ 
				propMod.mainController.selectionChanged(TypeTableModel.this,propMod,Array(newInst))
				selfAdded=false
			}
		}
			//table.peer.setRowSelectionInterval(newSize-1,newSize-1)}
	}

	def removeInstance(ref:Reference) = listLock.synchronized{	
		//println("tablemod remove inst: "+ref)
		val pos=dataList.indexWhere(_.ref==ref)
		if(pos<0) println("Remove Instance "+ref+" not found !")
		else {
			dataList=dataList filterNot(_.ref ==ref)
			selectedInstances.buf=dataList
			calcSize
			/*propMod.runSw*/fireTableRowsDeleted(pos,pos)
		}
	}


	def getRowCount= listLock.synchronized{
		 //println("get size "+(dataList.size+1)+ " " +Thread.currentThread)
		if(dataList!=null) dataList.size+1
		else 0
	}

	def getColumnCount= listLock.synchronized{
		objClass.fields.size+1
	}

	def getValueAt(row:Int,col:Int):Object = listLock.synchronized{
		if(dataList!=null&& row<dataList.size) {
			if(col==0) { if (dataList(row).hasChildren) "+" else " " } // childInfo in column 0
			else {
				val expr=dataList(row).fieldData(col-1)
				if(expr.isConstant) expr
				else expr.getTerm+": "+expr.getValue
			}
		}
		else null
	}

	override def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit =		
		if(dataList!=null&& columnIndex >0)  listLock.synchronized {  		
			val expr=parseValue(columnIndex,aValue) 		
			if(rowIndex==dataList.size) { // create new
				selfAdded=true
				val id=ClientQueryManager.createInstance(typ,Array(propMod.getOwnerRef))				
				ClientQueryManager.writeInstanceField(Reference(typ,id),(columnIndex-1).toByte,expr)											
			}
			else {
				val ref=dataList(rowIndex).ref
				ClientQueryManager.writeInstanceField(ref,(columnIndex-1).toByte,expr)
			} 		
			
		}  	


	override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
		columnIndex>0 
	}

	override def getColumnName(col:Int) = listLock.synchronized {
		if(col==0) "ch" else objClass.fields(col-1).name
	}


	def parseValue(columnIndex:Int,value:Object):Expression = {
		if(value==null)EMPTY_EX else
			if (objClass.fields(columnIndex-1).typ==DataType.StringTyp)
				try {
					StringParser.parse( value.toString) 
				} 
		catch {
			case _ => new StringConstant(value.toString)				
		}
		else StringParser.parse( value.toString) // throw exception when fail
	}


	def deselect() = listLock.synchronized{
		//println("deselect " +typ+ " rows:"+table.selection.rows)
		if(dataList!=null && (! table.selection.rows.isEmpty)) {
			selfSelectChanged=true
			table.peer.clearSelection
			selfSelectChanged=false
			selectedInstances.setFilter(Array())
		}

	}

	


	class SelectList[A](var buf: Seq[A],private var filterSet:Array[Int]=Array()) 
	extends collection.immutable.IndexedSeq[A] {
		def length = if(buf==null) 0 else filterSet.size
		def apply(idx: Int) ={ //println ("buf size:"+buf.size+ " "+" idx:"+idx+" filterSet:"+filterSet.mkString+" "+Thread.currentThread)
			val bufIx=filterSet(idx)
			if(bufIx<buf.size)
			buf.apply(filterSet(idx))
			else null.asInstanceOf[A]
		}
		def setFilter(newFilter:Array[Int]) = if(buf!=null && newFilter.contains(buf.size)) {
			filterSet=newFilter.take(newFilter.size-1)
		} else filterSet=newFilter
	 }

  

}