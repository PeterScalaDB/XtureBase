/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import server.storage._
import definition.typ._
import definition.data._

/**
 * 
 */
object TypTableModel extends AbstractTableModel 
{
	var classList:Array [ObjectClass]=Array()
	
	def setClassList(newList:Array [ObjectClass]) = 
	{
		classList=newList
		fireTableStructureChanged()
	}
		
  def getRowCount():Int =
  {
		classList.length
  }
  
  def getColumnCount():Int = 3
  
  def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	val obj=classList(row)
  	column match {
  		case 0 => obj.id.asInstanceOf[AnyRef]
  		case 1 => obj.name
  		case 2 => obj.description  		
  		case _ => null
  	}
  }
  
  override def getColumnName(column:Int) =
  {
  	column match {
  		case 0 => "ID"
  		case 1 => "Name"
  		case 2 => "Beschreibung"  		
  		case _ => "***"
  	}
  }

}