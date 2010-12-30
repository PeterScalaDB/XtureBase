/**
 * Author: Peter Started:14.12.2010
 */
package management.databrowser
import scala.swing._
import java.awt.Font
import client.comm.ErrorListener
/**
 * 
 */
class ConsolePanel extends BorderPanel with ErrorListener{
	val textArea=new TextArea
	textArea.wordWrap=true
	textArea.lineWrap=true
	val textFont=new Font("Courier New",0,12)
	textArea.font=textFont
	textArea.editable=false
	
	add(new ScrollPane {
		viewportView=textArea
	},BorderPanel.Position.Center)

	def printError(errorText:String)={		
		textArea.append(errorText)
	}
}