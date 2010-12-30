/**
 * Author: Peter Started:18.12.2010
 */
package client.dialog
import javax.swing._

/**
 * 
 */
class ComboBoxEditor(box:JComboBox) extends DefaultCellEditor(box) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {  		
  		val editor:JComboBox =super.getTableCellEditorComponent( table, value, isSelected, row, column ).asInstanceOf[JComboBox]; 
  		editor.setSelectedItem( value )
  		editor
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		getComponent().asInstanceOf[JComboBox].getSelectedItem();  		
  	}
  }