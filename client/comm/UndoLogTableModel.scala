/**
 * Author: Peter Started:03.11.2010
 */
package client.comm
import definition.data._
import javax.swing._
import javax.swing.table._
import java.awt.{Dimension,Font}
import scala.swing._
import scala.swing.event._
import java.text.{DateFormat}
import definition.typ._

/** Table model for showing the undo steps 
 * 
 */
class UndoLogTableModel extends AbstractTableModel {
	val dFormat=DateFormat.getDateInstance(DateFormat.SHORT)
	val tFormat=DateFormat.getTimeInstance(DateFormat.SHORT)

	
	var stepList:Seq[TransStepData]=Seq.empty
	
	def getRowCount= {
		stepList.size
	}
	
	def loadStepList(newList:Seq[TransStepData]) = {
		stepList=newList
		fireTableDataChanged()
	}
	
	def getColumnCount= 4
	
	def getValueAt(row:Int,col:Int):Object = {
		if(row<stepList.size) {
			val step=stepList(row)
			col match {
				case 0 => step.trID.toString
				case 1 => step.userID
				case 2 => { val da=new java.util.Date(step.time*60000L)
					dFormat.format(da)+" | "+tFormat.format(da)
				}
				case 3 => if(step.createType >0) {
					AllClasses.get.getClassByID(step.createType).name+"-Objekt erzeugt in ("+
					AllClasses.get.getClassByID(step.firstInst.ref.typ).name+") "+step.firstInst.toString
				} else {
					step.action+" ("+AllClasses.get.getClassByID(step.firstInst.ref.typ).name+") "+
					step.firstInst.toString+" "+(if(step.multiInst) " und weiteren" else "")
				}
			}
		}
		else null
	}
	
	override def getColumnName(col:Int) = {
		col match {
			case 0 => "Schritt"
			case 1 => "Benutzer"
			case 2 => "Zeit"
			case 3 => "Durchgeführte Aktion"
			case _ => ""
		}
	}

}