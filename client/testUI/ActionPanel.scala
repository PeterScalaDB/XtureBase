/**
 * Author: Peter Started:25.09.2010
 */
package client.testUI

import scala.swing._
import definition.typ._
import scala.swing.event._
import collection.mutable.ArrayBuffer
import definition.data._
import client.dataviewer._

/** a panel showing all possible panels
 * 
 */
class ActionPanel extends BoxPanel(scala.swing.Orientation.Vertical) with SelectListener {
	//var actionList:Seq[AbstractAction]= _
	val buttonList= new ArrayBuffer[ActionButton]()
	var usedButtons:Int= _
	var lastClass:Int= _
	var buttonSize=new Dimension(110,30)
	var nullSize=new Dimension(0,0)
	
	reactions += {
		case ButtonClicked(e:ActionButton) => {
			println("Button " +e.text+" clicked")
		}
	
	}
	
	def selectionChanged(instList:Seq[InstanceData]) = {
		//println("select "+instList)
		if(instList==null || instList.isEmpty) hideActions
		else setClass(instList.first.ref.typ)
	}
	
  def setClass(classID:Int):Unit = {
  	visible=true
  	xLayoutAlignment=0.5d
  	yLayoutAlignment=0.5d
  	if (classID==lastClass) return // keep the actions 
  	shutDown()  
  	
  	val theClass = AllClasses.get.getClassByID(classID)
  	//println("class "+classID+" num:"+theClass.getActionCount)
  	for(a <-0 until theClass.getActionCount)
  		contents += getButton(theClass.action(a)) 
  	revalidate
  	repaint
  }
  
  def hideActions = {
  	visible=false
  }
	
	
	def shutDown() = {
		contents.clear()		
		usedButtons=0		
	}
	
	
	def getButton(action:AbstractAction):ActionButton = {
		val retButton= if(buttonList.size>usedButtons) buttonList(usedButtons)
			 else {
				 val newBut=new ActionButton
				 buttonList.append(newBut)
				 listenTo(newBut)
				 newBut
			 }
		
		retButton.text=action.name
		retButton.question=action.question
		usedButtons += 1
		retButton
	}
 
	class ActionButton extends Button {	
		var question:Option[ParamQuestion]=_	
		preferredSize=buttonSize		
		maximumSize=buttonSize
		xLayoutAlignment=0.5d
}
	
  
}

