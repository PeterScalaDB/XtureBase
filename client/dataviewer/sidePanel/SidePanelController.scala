/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel
import definition.data._
import scala.swing._
import definition.typ.AbstractObjectClass
import client.dataviewer.TypeTableModel


// the class that contains the controller
trait ControllerContainer {
	def closeSideBar:Unit
}

/** abstract controller for a side panel next to a TypeTable
 * 
 */


trait SidePanelController {
	// does the data loaded in the table fit for the side panel
	
	def classFits(tableClass:AbstractObjectClass):Boolean
  def parentsFits(dataModel:TypeTableModel,parentRef:Reference):Boolean
  
  
  def panelName:String
    
  /** load data in the side panel 
   * 
   * @param parentRef Reference of the parent instance in the path panel
   * @param tableType the type of the data instances in the TypeTable
   * 
   */
  def openPanel(parentRef:Reference,tableClass:AbstractObjectClass,container:ControllerContainer):Unit
  
  //def shutDown:Unit
  
  def closePanel:Unit
  
  def headerComp:Component
  
  def mainComp:Component  
  
  /** notifies the Controller that the number of y-rows has changed
   * 
   */
  def notifyRowsChanged:Unit
}