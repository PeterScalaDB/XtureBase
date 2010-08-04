/**
 * Author: Peter Started:27.07.2010
 */
package management.databrowser

import scala.swing._
import scala.swing.event._



/** Shows the Transaction Log Table
 * 
 */
object TransLogPanel extends BorderPanel {
	
	add (new Label ("Trasaction Log File "),BorderPanel.Position.North)
	
  add( new ScrollPane() {
  	viewportView= new Table()
  	{
  		model=LogFileModel
  	}
  },BorderPanel.Position.Center)
  
  add ( new GridPanel(1,1){
  	val refBut= new Button("Refresh")
  	contents += refBut
  	listenTo(refBut)
  	reactions+= {case ButtonClicked(`refBut`) => LogFileModel.refresh() }
  },BorderPanel.Position.South )
}