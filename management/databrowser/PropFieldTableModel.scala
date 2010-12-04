/**
 * Author: Peter Started:29.11.2010
 */
package management.databrowser

import definition.typ._
import definition.data._
import server.storage.ServerObjectClass
/**
 * 
 */
class PropFieldTableModel(val showLastLine:Boolean) extends ActivableAbstractTableModel {
  
	var propFieldList:Seq[PropertyFieldDefinition]=Seq.empty
	
	def setValues(npList:Seq[PropertyFieldDefinition])= {
		propFieldList=npList
		fireTableDataChanged
		isDirty=false
	}
	
	def updateList(newList:Seq[PropertyFieldDefinition]) = {
		propFieldList=newList
		isDirty=true
	}
	
	override def isCellEditable(row:Int,col:Int):Boolean = {  	
  	 showLastLine  	    	
  }
	
  def getRowCount(): Int = { propFieldList.size+(if(showLastLine)1 else 0) }

  def getColumnCount(): Int = { 3 }

  def getValueAt(row: Int, col: Int): Object = {
  	if(row>=propFieldList.size) null
  	else {  		
  		 val fd=propFieldList(row)
  		 col match {
  			 case 0 => fd.name 
  			 case 1 => fd.single.asInstanceOf[AnyRef]
  			 case 2 => fd.allowedClass.asInstanceOf[AnyRef]
  		 }
  		}
  }
  
  override def setValueAt(value:Object,row:Int,column:Int):Unit= { 
  	if(row==propFieldList.size&&showLastLine) { // create
  		propFieldList =propFieldList:+ new PropertyFieldDefinition("")
  	}
  	val ov=propFieldList(row)
  	//println("Set Value At:"+value+" row:"+row+" col:"+column)
  	propFieldList = MainWindow.updateSeq(propFieldList,row,
  		column match {
  			case 0 => ov.setName(value.toString)
  			case 1 => ov.setSingle(value.asInstanceOf[Boolean].booleanValue)
  			case 2 => ov.setAllowedClass(value.asInstanceOf[Integer].intValue)
  			case _ => ov
  		})
  	isDirty=true  	
  	fireTableDataChanged
  }
  
  override def getColumnClass(col:Int):Class[_] = {
  	col match {  		
  		case 0 => classOf[String]
  		case 1 => classOf[Boolean]
  		case 2 => classOf[Int]
  		case _ => classOf[String]
  	}
  }

  override def getColumnName(col:Int)= {
  	col match {  		
  		case 0 => "FieldName"
  		case 1 => "Single"
  		case 2 => "Allowed Class"
  		case _ => "*"
  	}
  } 
  
  def getPropField(row:Int)=  propFieldList(row)
  
  def update(theClass:ServerObjectClass) = if(isDirty){  	
  	if(showLastLine)
  		theClass.ownPropFields=propFieldList
  }
}