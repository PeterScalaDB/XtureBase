/**
 * Author: Peter Started:03.11.2010
 */
package client.comm

import scala.swing._
import scala.swing.event._
import definition.data.TransStepData
import java.awt.event.{WindowAdapter,WindowEvent}
/**
 * 
 */
class UndoDialog(w:Window) extends Dialog(w) with StepListReader {
	val prefSize=new Dimension(400,300)
  preferredSize=prefSize
  modal=true
  title="Zuletzt ausgeführte Aktionen"
  
  val undoTableModel=new UndoLogTableModel
  val undoBut=new Button("Letzte Aktion rückgängig")
	val cancelBut=new Button("Abbruch")
  
	
	
	
	peer.addWindowListener (new WindowAdapter(){
		
		override def windowClosing(e:WindowEvent)= {
			ClientQueryManager.stopUndo
		}
	}
	)

	listenTo(this)
	reactions+= {
		case e:WindowClosed => println("Window closed "+e) 
	}
	
  val undoTable=new Table(){
		model=undoTableModel
		autoResizeMode=Table.AutoResizeMode.LastColumn  
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None
		
	}
  	
  val mainPanel=new BorderPanel(){
		add(new ScrollPane () {
			viewportView = undoTable
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+=undoBut += cancelBut
		},BorderPanel.Position.South)
		listenTo(undoBut,cancelBut)
		reactions += {
			case ButtonClicked(`undoBut`)=> doUndo()
			case ButtonClicked(`cancelBut`)=>doCancel()
		}
		
	}
  
  def loadStepList(list:Seq[TransStepData]) = {
  	undoTableModel.loadStepList(list)
  	visible=true
  }
  
  
  contents=mainPanel
  val colMod=undoTable.peer.getColumnModel()
  undoTable.autoResizeMode=Table.AutoResizeMode.LastColumn
	colMod.getColumn(0).setPreferredWidth(50)
	colMod.getColumn(0).setMaxWidth(50)
	colMod.getColumn(1).setMaxWidth(70)
	colMod.getColumn(2).setMaxWidth(100)
	colMod.getColumn(2).setPreferredWidth(100)
  
  def doUndo() = {
  	ClientQueryManager.doUndo
  	close
  }
  
  def doCancel() = {
  	ClientQueryManager.stopUndo
  	close
  }  
  
  
}