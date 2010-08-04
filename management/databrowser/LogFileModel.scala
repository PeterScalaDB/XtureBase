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
  	 
   
   def getColumnCount:Int = 6
   
   def getValueAt(row:Int,col:Int) = {
  	 val rec=transList(row)
  	 col match {
  		 case 0 => TransType(rec._1)
  		 case 1 => rec._2.asInstanceOf[AnyRef]
  		 case 2 => rec._3.asInstanceOf[AnyRef]
  		 case 3 => rec._4.asInstanceOf[AnyRef]
  		 case 4 => rec._5.asInstanceOf[AnyRef]
  		 case 5 => rec._6.asInstanceOf[AnyRef]
  		 case _ => "**"
  	 }
   }
   
   override def getColumnName(col:Int) = col match {
  	 case 0 => "Trans-Type "
  	 case 1 => "TransID"
  	 case 2 => "Obj-Type"
  	 case 3 => "Obj-Inst"
  	 case 4 => "Data pos"
  	 case 5 => "Data length"
  	 case _ => "**"
   }
  	 
}