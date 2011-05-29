/**
 * Author: Peter Started:27.05.2011
 */
package client.dialog

import definition.expression.VectorConstant 

/**
 * 
 */
trait AbstractViewController {
	
	def startBracketMode():Unit
	
	def stopBracketMode():Unit
	
	def addDelta(x:Double,y:Double,z:Double):Unit
	
	def requestFocus():Unit
	
	def setCoordinate(x:Double,y:Double,z:Double):Unit
	
	def askForPointClick(plistener:PointClickListener)	
	
	def askForLineTo(plistener:PointClickListener,constraints:String)
	
	def askForObjectSelection(listener:ObjectSelectListener,constraints:String)
	
	def deselect():Unit
	
	def cancelModus():Unit
}