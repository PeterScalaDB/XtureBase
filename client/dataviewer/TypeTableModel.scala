/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import definition.typ._
import definition.data._
import javax.swing._
import javax.swing.table._
import java.awt.Dimension
import scala.swing._
import scala.swing.event._
import definition.expression._
import transaction.parser._
import client.comm._

/** table model for a table showing instances of a certain type
 * 
 */
class TypeTableModel(val typ:Int,propMod:PropertyModel) extends AbstractTableModel {
	val classVers=AllClasses.getClassByID(typ).lastVersion
	var dataList:Seq[InstanceData]= _
	
	val table=new Table(){		
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row
		listenTo(selection)		
		reactions += {
			case TableRowsSelected(table,range,live) => {
			
			if (!live)
				if(selection.rows.isEmpty) propMod.mainController.selectionChanged(TypeTableModel.this,propMod,null)
				else {
					val ix=selection.rows.first				
					if(dataList!=null && dataList.size>ix) propMod.mainController.selectionChanged(TypeTableModel.this,propMod,dataList(ix))
				}
			}		
				
			//println("#####################################")
			//Thread.dumpStack
		}
	}
	table.model=this
	
	val scroller=new ScrollPane() {
		viewportView=table
		preferredSize=new Dimension(100,100)
	}
	
	def setDataList(data:Seq[InstanceData]) = {
		dataList=data		
		fireTableStructureChanged()
	}
	
	def changeInstance(newInst:InstanceData) = {
		val pos=dataList.findIndexOf(_.ref==newInst.ref)
		if(pos<0) println("prop"+propMod.propertyField+"Table typ: "+typ+" Change Instance "+newInst.ref+" not found !")
		else { 
			dataList=dataList.updated(pos,newInst)
			fireTableRowsUpdated(pos,pos)
		}		
		
	}
	
	def addInstance(newInst:InstanceData) = {
		if(dataList==null) dataList=IndexedSeq(newInst)
		else dataList=dataList :+ newInst
		val newSize=dataList.size-1
		fireTableRowsInserted(newSize,newSize)
	}
	
	def removeInstance(ref:Reference) = {		
		val pos=dataList.indexWhere(_.ref==ref)
		if(pos<0) println("Remove Instance "+ref+" not found !")
		else {
			dataList=dataList filterNot(_.ref ==ref)
			fireTableRowsDeleted(pos,pos)
		}
	}
	
	
	def getRowCount= {
		if(dataList!=null) dataList.size+1
		else 0
	}
	
	def getColumnCount= {
		classVers.getFieldCount
	}
	
	def getValueAt(row:Int,col:Int) = {
		if(dataList!=null&& row<dataList.size) {
			val expr=dataList(row).fieldData(col)
			if(expr.isConstant) expr
			else expr+":"+expr.getValue
		}
		else null
	}
	
	override def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit = {  	
  	if(dataList!=null) {  		
  		val expr=if(aValue==null)EMPTY_EX else StringParser.parse( aValue.toString)  		
  		if(rowIndex==dataList.size) { // create new
  			val id=ClientQueryManager.createInstance(typ,Array(propMod.getOwnerRef))
  				ClientQueryManager.writeInstanceField(Reference(typ,id),columnIndex.toByte,expr)
  			}
  			else {
  				val ref=dataList(rowIndex).ref
  				ClientQueryManager.writeInstanceField(ref,columnIndex.toByte,expr)
  			} 		
  	}  	
  }
	
	override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
  	true 
  }
	
	override def getColumnName(col:Int) = {
		classVers.field(col).name
	}
	
	

}