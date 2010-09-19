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
	var dataList:Option[IndexedSeq[InstanceData]] = None
	var subscriptionID= -1
	var allowedClass=0
	
	private val listenerList:EventListenerList = new EventListenerList();
	
	def loadData(nparentRef:Reference,npropField:Byte) =
		//ClientQueryManager.runInPool( 
	{
		
		parentRef=nparentRef
		propField=npropField
		val parentClassVers=AllClasses.getClassByID(parentRef.typ).lastVersion
		allowedClass=parentClassVers.propField(propField).allowedClass
		classVers=AllClasses.getClassByID(allowedClass).lastVersion
		
		var updateStructure=true
		
		if(subscriptionID<0)
		subscriptionID=ClientQueryManager.createSubscription(parentRef,propField) {
			(notType:NotificationType.Value,data: IndexedSeq[InstanceData]) => {
				
				println("Table modification :"+notType+ " "+(if(data.isEmpty)" [Empty] "else   data.first.ref)+", ... "+Thread.currentThread.getName +
						" subsID:"+subscriptionID)
				//println()
				if(notType== NotificationType.sendData) dataList=Some(data)
				else for(list<- dataList)
					notType match {				  
						case NotificationType.childAdded => dataList= Some(list :+ data(0))
						case NotificationType.FieldChanged => {
							val newRef=data(0).ref
							for(i <-list.indices)
								if(newRef==list(i).ref) dataList=Some(list updated (i,data(0)))
						}
							
						case NotificationType.instanceRemoved => {
							if(data(0).ref==parentRef) {
								dataList=None
							}
							else dataList=Some(list.filter(_.ref != data(0).ref))	
						}
						
					}
				
				if(updateStructure ) { SwingUtilities.invokeLater(new Runnable {
				def run () =	fireTableStructureChanged();
				})
				updateStructure=false }
				else fireTableDataChanged()
			}			
		}
		else { // subscription already there
			ClientQueryManager.changeSubscription(subscriptionID,parentRef,propField)
		}
		
	}//)
	
	def shutDown() = {
		if(subscriptionID>=0)	{
			ClientQueryManager.removeSubscription(subscriptionID)
			subscriptionID= -1
		}
	}
	
	/*def replaceInstance(array:Array[InstanceData],elem:InstanceData):Array[InstanceData] =
		array.map(a => if (a.ref == elem.ref)elem else a).toArray*/
	

  def getRowCount(): Int = {
  	for(list <-dataList) return list.length+1 
  	return 0   	
  }

  def getColumnCount(): Int = { 
  	if(classVers==null) 0
  	else classVers.getFieldCount+1
  }

  def getColumnName(columnIndex: Int): String = { 
  	if(classVers==null) ""
  	else 
  		if(columnIndex==0) "Ref"
  		else classVers.field(columnIndex-1).name  		
  }

  def getColumnClass(columnIndex: Int): Class[String] = { classOf[String] }

  def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
  	columnIndex>0 
  }
  
  def getInstance(ix:Int):InstanceData = {
  	for(list<-dataList) return list(ix)
  	return null
  }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
  	for(list <-dataList) {
  	  if(rowIndex>=list.size) return null
  	  if(columnIndex==0) return list(rowIndex).ref.sToString
  	  if(columnIndex>classVers.getFieldCount) return null
  	  return list(rowIndex).fieldData(columnIndex-1).getValue.toString	
  	}
  	return null
  	
  }

  def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit = {
  	
  	if(columnIndex>0 && dataList!=null) {
  		val fieldNr=columnIndex-1
  		val expr=if(aValue==null)EMPTY_EX else StringParser.parse( aValue.toString)
  		for(list <-dataList) {
  			if(rowIndex==list.size) { // create new
  				val id=ClientQueryManager.createInstance(allowedClass,Array(new OwnerReference(propField,parentRef)))
  				ClientQueryManager.writeInstanceField(Reference(allowedClass,id),fieldNr.toByte,expr)
  			}
  			else {
  				val ref=list(rowIndex).ref
  				ClientQueryManager.writeInstanceField(ref,fieldNr.toByte,expr)
  			}
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
  	fireTableChanged(new TableModelEvent(SimpleTableModel.this));  			
  }
  def fireTableStructureChanged() { 	  
  	fireTableChanged(new TableModelEvent(SimpleTableModel.this, TableModelEvent.HEADER_ROW));
  }
  
  def fireTableChanged(e:TableModelEvent) {
  	//SwingUtilities.invokeLater(new Runnable() {
  	//	def run() {
  			val listeners:Array[Object] = listenerList.getListenerList();	
  		for (i <- (listeners.length-2) to (0,-2)) 
  			if (listeners(i)==classOf[TableModelListener]) 
  				(listeners(i+1).asInstanceOf[TableModelListener]).tableChanged(e);
  		//}
  	//})
  }
}