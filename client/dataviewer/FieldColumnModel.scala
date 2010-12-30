/**
 * Author: Peter Started:28.11.2010
 */
package client.dataviewer

import javax.swing.table._

/**
 * 
 */
class FieldColumnModel extends DefaultTableColumnModel {
	
  def createColumn(nr:Int,name:String,width:Int,editor:TableCellEditor=null) ={	 
		val nc=new TableColumn(nr)
		nc.setHeaderValue(name)
		nc.setPreferredWidth(width)
		nc.setWidth(width)
		if(editor!=null)nc.setCellEditor(editor)
		addColumn(nc)
	}

}