/**
 * Author: Peter Started:27.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel

import server.storage._

/**
 * 
 */
object LogFileModel extends AbstractTableModel {
	
   var transList=TransLogHandler.readFullIndex
   
   
   
   def getRowCount:Int = {
       transList.length 	 
   }
   
   def refresh() = {
  	 transList=TransLogHandler.readFullIndex
  	 fireTableStructureChanged()
   }
  	 
   
   def getColumnCount:Int = 7
   
   def getValueAt(row:Int,col:Int) = {
  	 val rec=transList(row)
  	 col match {
  		 case 0 => rec.transTyp
  		 case 1 => rec.trID.asInstanceOf[AnyRef]
  		 case 2 => rec.userID.asInstanceOf[AnyRef]
  		 case 3 => rec.typ.asInstanceOf[AnyRef]
  		 case 4 => rec.inst.asInstanceOf[AnyRef]
  		 case 5 => rec.dataPos.asInstanceOf[AnyRef]
  		 case 6 => rec.dataLength.asInstanceOf[AnyRef]
  		 case _ => "**"
  	 }
   }
   
   override def getColumnName(col:Int) = col match {
  	 case 0 => "Trans-Type "
  	 case 1 => "TransID"
  	 case 2 => "UserID"
  	 case 3 => "Obj-Type"
  	 case 4 => "Obj-Inst"
  	 case 5 => "Data pos"
  	 case 6 => "Data length"
  	 case _ => "**"
   }
  	 
}