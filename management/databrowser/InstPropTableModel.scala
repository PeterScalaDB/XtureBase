/**
 * Author: Peter Started:01.08.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import server.storage._
import transaction.parser.StringParser
import transaction.handling._

/**
 * 
 */
object InstPropTableModel extends AbstractTableModel {
	
	var theClass:ObjectClass=null
	var theVersion:ClassVersion=null
	var propData:Option[InstanceProperties]=None
	
	
	def setClass(newClass:ObjectClass) = 
	{
		theClass=newClass
		fireTableStructureChanged()
	}
	
	
	def setPropData(nprop:Option[InstanceProperties],versionNr:Byte)
	{
		theVersion=theClass.getVersion(versionNr) match {case Some(a) => a;case None =>null}
		propData=nprop
		println("propData "+propData)
		
  	//println("Set inst "+theVersion)
  	fireTableStructureChanged()
	}
	
	
	def getRowCount():Int =
  {
     if(theVersion==null) 0
     else theVersion.getPropFieldCount    
  }
	
	
	def getColumnCount():Int = 2
	
	
	def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	if(theVersion==null) " "
  	else 
    if(column==0)
    {  	
    	  theVersion.propField(row).name      	
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