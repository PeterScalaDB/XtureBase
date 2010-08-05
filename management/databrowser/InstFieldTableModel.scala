/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import server.storage._
import transaction.parser.StringParser
import transaction.handling._

/** Table model for the Instance Field table
 * 
 */
object InstFieldTableModel extends AbstractTableModel 
{
  var theClass:ObjectClass=null
  var instance:InstanceData=null
  var theVersion:ClassVersion=null
	
	def setClass(newClass:ObjectClass) = 
	{
		theClass=newClass
		fireTableStructureChanged()
	}
  
  def setInstance(newInst:InstanceData) =
  {
  	println("instmodel set Instance "+newInst)
  	instance=newInst
  	if(instance==null) theVersion=null
  	else theVersion=theClass.getVersion(instance.classVersion) match {case Some(a) => a;case None =>null}
  	//println("Set inst "+theVersion)
  	fireTableStructureChanged()
  }
		
  def getRowCount():Int =
  {
     if(theVersion==null) 0
     else theVersion.getFieldCount+ 2     
  }
     
  override def isCellEditable(rowIndex:Int,columnIndex:Int) = (rowIndex>1) && (columnIndex==1)
  

  
  def getColumnCount():Int = 3
  
  def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	if(theVersion==null) " "
  	else 
    column match {
  		case 0 =>    	row match{
    		case 0 => "Version"
    		case 1 => "Owner"
    		case _ => theVersion.field(row-2).name    	    	
      }
  		case 1 => row match{
    		case 0 => instance.classVersion.asInstanceOf[AnyRef]
    		case 1 => instance.owners.mkString(", ")
    		case _ => instance.fieldData(row-2).getTerm 
    	}
  		case 2 => if(row<2) " " else { 
  			instance.fieldValue(row-2).toString
  		}
  		case _ => "bla"
  	}
    
    
  }
  
  override def setValueAt(obj:Object,row:Int,column:Int) =
  {
  	if((row>1)&&(column==1)&& instance!=null)
  	{
  		val f:Byte=(row-2).toByte  		
  		
  		TransactionManager.doTransaction {	
  			TransactionManager.tryWriteInstanceField(instance.ref,f,StringParser.parse(obj.toString))
  		}
  		
  	}
  }
  
  override def getColumnName(column:Int) =
  {
  	column match {
  		case 0 => "Field"
  		case 1 => "Term"
  		case 2 => "Value"
  		case _ => "***"
  	}
  }
}