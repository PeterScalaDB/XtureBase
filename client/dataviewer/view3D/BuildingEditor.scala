/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D

import scala.swing._
import definition.typ.CustomInstanceEditor
import definition.data.Reference
import java.awt.Dimension
import client.comm.ClientQueryManager

/**
 * 
 */
class BuildingEditor extends BorderPanel with CustomInstanceEditor {
  
	
	preferredSize=new Dimension(100,400)
	minimumSize=preferredSize
	
	val textEdit=new TextArea
	
	add(textEdit,BorderPanel.Position.Center)
	
  def getComponent(): Component = this
  
  var workArea:Option[WorkArea]=None

  def load(ref: Reference): Unit = {  
  	workArea=Some(new WorkArea(ref))
  	workArea.get.addPrismListener(workAreaLoaded)
  }

  def shutDown(): Unit = if(workArea.isDefined){  
  	workArea.get.shutDown()
  	workArea=None
  }
  
  def workAreaLoaded(data:Option[BorderPrism]):Unit = ClientQueryManager.runSw{
  	data match {
  		case Some(d)=>textEdit.text=d.toString
  		case None=> textEdit.text="No prism"
  	}
  }

  def editorName(): String = "BuildingEditor"

}