/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import definition.typ._
import definition.data._
import javax.swing._
import javax.swing.table._
import java.awt.{Dimension,Font}
import scala.swing._
import scala.swing.event._
import definition.expression._
import transaction.parser._
import client.comm._
import collection.generic.{CanBuildFrom, GenericTraversableTemplate,
	GenericCompanion, SeqFactory}
import collection.mutable.{Builder,ArrayBuffer}
import collection.immutable.Vector
import java.awt.Color


/** table model for a table showing instances of a certain type
 * 
 */
class TypeTableModel(val typ:Int,propMod:PropertyModel) extends AbstractTableModel {
	val objClass=AllClasses.get.getClassByID(typ)
	var dataList:Seq[InstanceData]= _
	val tableFont=new Font("Arial",0,15)
	var selfSelectChanged=false
	var selfAdded=false
	val selectedInstances=new SelectList(dataList)

	val listLock=new Object

	val table=new Table(){		
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row    		
		rowHeight=20
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
	}
	table.model=this	
	val colMod=table.peer.getColumnModel()
	if(colMod.getColumnCount>0) colMod.getColumn(0).setPreferredWidth(30)

	val scroller=new ScrollPane() {
		viewportView=table
		preferredSize=new Dimension(100,100)	
		
	}

	def setDataList(data:Seq[InstanceData],selectInstance:Option[Reference]) =  {
		listLock.synchronized {
			
			dataList=data
			calcSize()
			selfSelectChanged=false
			selfAdded=false
			selectedInstances.buf=data
			selectedInstances.setFilter(Array())
		}		 
			fireTableStructureChanged()
			selectInstance match {
				case Some(ref) =>if(ref.typ == typ){
					val ix= dataList.findIndexOf(_.ref==ref)
					if(ix>=0)  table.selection.rows+=ix				
				}
				case _ =>
			}
		val colMod=table.peer.getColumnModel()
		colMod.getColumn(0).setMaxWidth(30)
		colMod.getColumn(0).setPreferredWidth(30)
	}
	
	def calcSize() = {
		
		val tabPrefHeight=table.preferredSize.height+30
		val mainPanelHeight=propMod.mainController.panel.size.height
		scroller.preferredSize=new Dimension(100,if(tabPrefHeight>mainPanelHeight-30)mainPanelHeight-30 else tabPrefHeight )
		//scroller.preferredSize=new Dimension(100,(if (dataList==null || dataList.isEmpty)2 else dataList.size+2)*22)
		scroller.minimumSize=scroller.preferredSize
		scroller.revalidate
	}

	def changeInstance(newInst:InstanceData):Unit = listLock.synchronized {
		
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
		if(dataList==null) dataList=IndexedSeq(newInst)
		else {
			dataList=dataList :+ newInst			
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
				else expr+":"+expr.getValue
			}
		}
		else null
	}

	override def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit =		
		if(dataList!=null&& columnIndex >0)  {  		
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
			case e:Exception =>  				
			new StringConstant(value.toString)				
		}
		else StringParser.parse( value.toString) // throw exception when fail
	}


	def deselect() = {
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