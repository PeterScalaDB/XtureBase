/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import definition.expression.StringParser
import server.storage._
import transaction.handling._
import definition.comm._

/** Table model for the Instance Field table
 * 
 */
object InstFieldTableModel extends AbstractTableModel 
{
  var theClass:AbstractObjectClass=null
  var instance:InstanceData=null
  
	
	def setClass(newClass:AbstractObjectClass) = 
	{
		theClass=newClass
		instance=null
		fireTableStructureChanged()
	}
  
  def setInstance(newInst:InstanceData) =
  {
  	//System.out.println("instmodel set Instance "+newInst)
  	instance=newInst  	
  	//System.out.println("Set inst "+theVersion)
  	fireTableStructureChanged()
  }
		
  def getRowCount():Int =
  {
     if(theClass==null) 0
     else theClass.fields.size+ 2     
  }
     
  override def isCellEditable(rowIndex:Int,columnIndex:Int) = (rowIndex>0) && (columnIndex==1)
  

  
  def getColumnCount():Int = 3
  
  def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	if(theClass==null) " "
  	else 
    column match {
  		case 0 =>    	row match{
    		
    		case 0 => "Owner"
    		case 1 => "SU-Owner"
    		case _ => theClass.fields(row-2).name    	    	
      }
  		case 1 => if(instance==null) " " else  row match{
    		
    		case 0 => instance.owners.mkString(", ")
    		case 1 => instance.secondUseOwners .mkString(", ")
    		case _ => if(row<=instance.fieldData.size) instance.fieldData(row-2).getTerm else " " 
    	}
  		case 2 => if(instance==null || row<2) " " else { 
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
  		
  		TransactionManager.doTransaction(0,ClientCommands.writeField.id.toShort,instance.ref,false,0, {	
  			TransactionManager.tryWriteInstanceField(instance.ref,f,StringParser.parse(obj.toString))
  		})
  		
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