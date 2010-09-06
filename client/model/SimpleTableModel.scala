/**
 * Author: Peter Started:05.09.2010
 */
package client.model

import javax.swing.event._
import javax.swing.table._
import javax.swing._
import definition.data._
import definition.typ._
import client.comm._
import definition.comm._
import transaction.parser._
import definition.expression._

/** a simple table model for subscriptions
 * 
 */
class SimpleTableModel extends TableModel {
	var parentRef:Reference=null
	var propField:Byte=0
	var classVers:ClassVersion=null
	var dataList:Array[InstanceData] = null
	var subscriptionID= -1
	var allowedClass=0
	
	private val listenerList:EventListenerList = new EventListenerList();
	
	def loadData(nparentRef:Reference,npropField:Byte) = {
		parentRef=nparentRef
		propField=npropField
		val parentClassVers=AllClasses.getClassByID(parentRef.typ).lastVersion
		allowedClass=parentClassVers.propField(propField).allowedClass
		classVers=AllClasses.getClassByID(allowedClass).lastVersion
		
		var updateStructure=true
		// remove subscription
		shutDown()
		subscriptionID=ClientQueryManager.createSubscription(parentRef,propField) {
			(notType:NotificationType.Value,data: Array[InstanceData]) => {
				
				println("modification :"+notType+ " "+data.map(a => a.ref.sToString).mkString(",")+" "+Thread.currentThread.getName)
				//println()
				notType match {
					case NotificationType.sendData => dataList=data
					case NotificationType.childAdded => dataList= dataList ++ data
					case NotificationType.FieldChanged => dataList=replaceInstance(dataList,data(0))
					case NotificationType.childRemoved => dataList=dataList.filter(_.ref != data(0).ref)
				}
				
				if(updateStructure ) { fireTableStructureChanged(); updateStructure=false }
				else fireTableDataChanged()
			}			
		}
		
	}
	
	def shutDown() = {
		if(subscriptionID>=0)	{
			ClientQueryManager.removeSubscription(subscriptionID)
			subscriptionID= -1
		}
	}
	
	def replaceInstance(array:Array[InstanceData],elem:InstanceData):Array[InstanceData] =
		array.map(a => if (a.ref == elem.ref)elem else a).toArray
	

  def getRowCount(): Int = {
  	if(dataList==null) 0 
  	else dataList.length+1  	
  }

  def getColumnCount(): Int = { 
  	if(dataList==null) 0
  	else classVers.getFieldCount+1
  }

  def getColumnName(columnIndex: Int): String = { 
  	if(dataList==null) null
  	else 
  		if(columnIndex==0) "Ref"
  		else classVers.field(columnIndex-1).name  		
  }

  def getColumnClass(columnIndex: Int): Class[String] = { classOf[String] }

  def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
  	columnIndex>0 
  }
  
  def getInstance(ix:Int) =dataList(ix)

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
  	if(dataList==null) return null
  	if(rowIndex>=dataList.size) return null
  	if(columnIndex==0) return dataList(rowIndex).ref.sToString
  	if(columnIndex>classVers.getFieldCount) return null
  	dataList(rowIndex).fieldData(columnIndex-1).getValue.toString
  }

  def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit = {
  	
  	if(columnIndex>0 && dataList!=null) {
  		val fieldNr=columnIndex-1
  		val expr=if(aValue==null)EMPTY_EX else StringParser.parse( aValue.toString)
  		if(rowIndex==dataList.size) { // create new
  			val id=ClientQueryManager.createInstance(allowedClass,Array(new OwnerReference(propField,parentRef)))
  			ClientQueryManager.writeInstanceField(Reference(allowedClass,id),fieldNr.toByte,expr)
  		}
  		else {
  			val ref=dataList(rowIndex).ref
  			ClientQueryManager.writeInstanceField(ref,fieldNr.toByte,expr)
  		}
  	}  	
  }

  def addTableModelListener(l: TableModelListener): Unit = {
  	listenerList.add(classOf[TableModelListener], l);
  }

  def removeTableModelListener(l: TableModelListener): Unit = {  
  	listenerList.remove(classOf[TableModelListener], l);
  }
  
  def fireTableDataChanged() {
        fireTableChanged(new TableModelEvent(this));
    }
  def fireTableStructureChanged() {
        fireTableChanged(new TableModelEvent(this, TableModelEvent.HEADER_ROW));
    }
  
  def fireTableChanged(e:TableModelEvent) {	
	  val listeners:Array[Object] = listenerList.getListenerList();	
		for (i <- (listeners.length-2) to (0,-2)) 
	    if (listeners(i)==classOf[TableModelListener]) 
	    	(listeners(i+1).asInstanceOf[TableModelListener]).tableChanged(e);	
  }
}