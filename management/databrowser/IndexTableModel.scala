/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel

import server.storage._


/** Table Model for the Index list for a class
 * 
 */
object IndexTableModel extends AbstractTableModel 
{
	var handler:ClassIndexHandler=null
	var ixList: Array [IndexRecord]=Array ()	
	
  def setTypeHandler(nhandler:ClassIndexHandler) =
  {
		handler=nhandler
		InstFieldTableModel.setClass(handler.theClass)
		InstPropTableModel.setClass(handler.theClass)
		readTheList()					
  }
	
	def readTheList()
	{
		System.out.println(handler.lastID)
		var ix=0
		ixList=handler.readFully									
		fireTableStructureChanged()
	}
	
	
  def getRowCount(): Int = { ixList.size }

  def getColumnCount(): Int = { 3 }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = 
  { 
  	if(rowIndex>=ixList.size) return null
  	val a =ixList(rowIndex) 
  	columnIndex match 
  	{
  		case 0 => a.inst.toString
  		case 1 => a.dataPos.toString
  		case 2 => a.dataLength.toString
  	}
  }
  
  override def getColumnName(col:Int) = 
  {
  	col match 
  	{
  		case 0 => "Inst-ID"
  		case 1 => "DataPos"
  		case 2 => "DataLength"
  	}
  }

}