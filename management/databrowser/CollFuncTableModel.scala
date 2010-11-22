/**
 * Author: Peter Started:07.09.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import server.storage._
import transaction.handling._

/**
 * 
 */
object CollFuncTableModel extends AbstractTableModel{
	var collData:Option[CollFuncResultSet]=None
	
	def setCollData(data:Option[CollFuncResultSet]) = {
		collData=data
		fireTableStructureChanged()
	}
	
	def getRowCount():Int =
  {
		for (a<-collData)
		{
			return a.callResultList.size
		}
		0
  }
	
	def getColumnCount():Int = 6
	
	def getValueAt(row:Int,column:Int):java.lang.Object = {
		for (a<-collData)
		{
			a.callResultList(row) match {
				case s:SingleCollFuncResult => {
					column match {
						case 0 => return s.funcName
						case 1 => return s.childType.toString
						case 2 => return s.childField.toString
						case 3 => return s.parentField.toString
						case 4 => return s.parentPropField.toString
						case 5 => return s.resultValue.toString
					}
				}
				case l:ListCollFuncResult => {
					column match {
						case 0 => return l.funcName
						case 1 => return l.childType.toString
						case 2 => return l.childField.toString
						case 3 => return l.parentField.toString
						case 4 => return l.parentPropField.toString
						case 5 => return l.resultList.mkString(",")
					}
				}
			}			
		}
		null
	}
	
	override def getColumnName(column:Int) =   {
		column match {
			case 0=> "funkName"
			case 1=> "childType"
			case 2=> "childField"
			case 3=> "parentField"
			case 4=> "parentPropField" 
			case 5=> "resultValue"
		}
  }

}