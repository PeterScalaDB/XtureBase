/**
 * Author: Peter Started:20.04.2011
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ.form.FormElement

/**
 * 
 */

trait DesignerListener {
	def initComponent(el:FormElement):Unit
	def changeElement(oldEl:FormElement,newEl:FormElement):Unit
}

class FormElementTableModel extends AbstractTableModel {

	var formElement:Option[FormElement]=None
	var props:List[(String,String)]=Nil
	
	var listener:Option[DesignerListener]= None
	
  def getRowCount(): Int = { 
		for(el <-formElement)
			return props.size  	
	  return 0
	}

  def getColumnCount(): Int = 2
  
  override def isCellEditable(row:Int,col:Int)= col==1

  
  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
  	  if(formElement.isDefined&& rowIndex < props.size && columnIndex<2){
  	  	val row=props(rowIndex)
  	  	if(columnIndex==0) row._1+":"
  	  	else row._2
  	  }
  	  else ""
  	}
  
  override def setValueAt(newValue:Object, rowIndex: Int, columnIndex: Int): Unit = 
  	if(rowIndex < props.size && columnIndex<2)
  	  for(oldEl<-formElement;li <-listener)
  	  {  	  	
  	  	println("set value "+newValue+" "+newValue.getClass)
  	  	val row=props(rowIndex) 
  	  	val newText=newValue match {
  	  		case t:Tuple2[_,_]=> t._2.toString
  	  		case other => other.toString
  	  	}
  	  	val newEl=oldEl.updateProperty(row._1, newText,Some(li.initComponent))  	  	
  	  	li.changeElement(oldEl,newEl)  	  	
  	  	setFormElement(Some(newEl))
  	  }
  	  
  	
  
  override def getColumnName(column:Int) =
  {
  	column match {
  		case 0 => "Field" 
  		case 1 => "Value"
  	}
  }  
  
  def setFormElement(el:Option[FormElement])= {
  	formElement=el
  	if(el.isDefined) 
  	  props=el.get.getProps
  	fireTableDataChanged
  }
  
  def isFieldField(row:Int)= {
  	val ix=props.findIndexOf(_._1=="field")
  	row==ix
  }
  
}