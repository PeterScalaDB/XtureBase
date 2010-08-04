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
	var ixList: Array [(Long,Long,Int)]=Array ()
	
  def setTypeHandler(nhandler:ClassIndexHandler) =
  {
		handler=nhandler
		InstFieldTableModel.setClass(handler.theClass)
		InstPropTableModel.setClass(handler.theClass)
		readTheList()					
  }
	
	def readTheList()
	{
		ixList=
		(for (i <- 1L to handler.lastID; if(handler.instanceExists(i)); rec=handler.getInstanceRecord(i) )
			yield (i,rec.dataPos,rec.dataLength)).toArray				
		fireTableStructureChanged()
	}
	
	
  def getRowCount(): Int = { ixList.length }

  def getColumnCount(): Int = { 3 }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = 
  { 
  	val a =ixList(rowIndex) 
  	columnIndex match 
  	{
  		case 0 => a._1.asInstanceOf[AnyRef]
  		case 1 => a._2.asInstanceOf[AnyRef]
  		case 2 => a._3.asInstanceOf[AnyRef]
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