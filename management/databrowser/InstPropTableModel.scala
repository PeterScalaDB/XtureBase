/**
 * Author: Peter Started:01.08.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import server.storage._
import definition.expression.StringParser
import transaction.handling._

/** Table Model for showing the property field values of an instance
 * 
 */
object InstPropTableModel extends AbstractTableModel {
	
	var theClass:AbstractObjectClass=null	
	var propData:Option[InstanceProperties]=None
	
	
	def setClass(newClass:AbstractObjectClass) = 
	{
		theClass=newClass
		fireTableStructureChanged()
	}
	
	
	def setPropData(nprop:Option[InstanceProperties])
	{		
		propData=nprop
		//System.out.println("propData "+propData)
		
  	//System.out.println("Set inst "+theVersion)
  	fireTableStructureChanged()
	}
	
	
	def getRowCount():Int =
  {
     if(theClass==null) 0
     else theClass.propFields.size
  }
	
	
	def getColumnCount():Int = 2
	
	
	def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	if(theClass==null) " "
  	else 
    if(column==0)
    {  	
    	  theClass.propFields(row).name      	
    }
    else
    {	    	
    	 propData match{
    		 case Some(a) => a.propertyFields(row)
    		 case _ => " "
    	 }
    }
  }
	
	override def getColumnName(column:Int) =
  {
  	column match {
  		case 0 => "PropertyField"
  		case 1 => "Owned Instances"  		
  		case _ => "***"
  	}
  }
	

}