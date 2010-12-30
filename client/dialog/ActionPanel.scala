/**
 * Author: Peter Started:25.09.2010
 */
package client.dialog

import scala.swing._
import definition.typ._
import scala.swing.event._
import collection.mutable.ArrayBuffer
import definition.data._
import client.dataviewer._
import client.comm.ClientQueryManager
import definition.expression.Constant
import javax.swing.BorderFactory
import javax.swing.border._

/** a panel showing all possible panels
 * 
 */

trait ActionPanListener {	
	/** starts the dialog to get parameters
	 * 
	 * @param actionName name of the action to start
	 * @param question set of questions and parameters
	 * @param createType create action of given Type
	 * @param actionInst the instances to do the action with	 
	 */	
	def startActionDialog(actionName:String,question:ParamQuestion,groupList:Seq[SelectGroup[_<:Referencable]],createType:Int)
	//def startNewDialog(actonName:String,question:ParamQuestion,func:(Seq[(String,Constant)])=>Unit)
}

class AbstractActionPanel extends BoxPanel(scala.swing.Orientation.Vertical)  {
	//var actionList:Seq[AbstractAction]= _
	val buttonList= new ArrayBuffer[ActionButton]()
	var usedButtons:Int= _
	var lastClass:Int= _
	var buttonSize=new Dimension(130,30)
	var nullSize=new Dimension(0,0)	
	
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)

	var listenerSet=collection.mutable.HashSet[ActionPanListener]()
	xLayoutAlignment=0.5d
  yLayoutAlignment=0.5d	
  
  def hideActions = {
  	visible=false
  }	
	
	def shutDown() = {
		contents.clear()		
		usedButtons=0		
	}	
	
	def getButton(theAction:AbstractAction,buttonLabel:String =""):ActionButton = {
		val retButton= if(buttonList.size>usedButtons) buttonList(usedButtons)
			 else {
				 val newBut=new ActionButton
				 buttonList.append(newBut)
				 listenTo(newBut)
				 newBut
			 }
		retButton.newTypeID=0
		if(buttonLabel!="") retButton.text=buttonLabel 
		else retButton.text=theAction.name
		retButton.theAction=theAction
		usedButtons += 1
		retButton
	}
	
	def registerActionPanListener(listener:ActionPanListener) = {
		listenerSet+=listener
	}
 
	class ActionButton extends Button {
		var newTypeID:Int=_
		var propField:Byte = _
		var theAction:AbstractAction=_	
		preferredSize=buttonSize		
		maximumSize=buttonSize
		focusable=false
		xLayoutAlignment=0.5d
		margin=new Insets(0,0,0,0)
	}  
}

class ActionPanel extends AbstractActionPanel with SelectListener {
	var lastSender:SelectSender= _
	var groupList:Seq[SelectGroup[_<:Referencable]]= _
	
	
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]]) = {
		//System.out.println("select "+instList)
		if(lastSender!=null&&lastSender!=sender)lastSender.deselect(false)
		groupList=groups
		lastSender=sender		
		
		if(groupList==null || groupList.isEmpty || groupList.first.children.isEmpty) hideActions
		else setClass(groupList.first.children.first.ref.typ)
	}	
	
	def setClass(classID:Int):Unit = {
		visible=true  	
		if (classID==lastClass) return // keep the actions 
		shutDown()   	
		val theClass = AllClasses.get.getClassByID(classID)  	
		//System.out.println("class "+classID+" num:"+theClass.getActionCount)
		for(a <-theClass.actions.valuesIterator)
			contents += getButton(a) 
		revalidate
		repaint
	}
	
	reactions += {
		case ButtonClicked(e:ActionButton) => if(groupList!=null){			
			if (e.theAction.question ==None ) {
				//System.out.println("Execute action " +e.text)
				//if(e.newTypeID>0) for(group<-groupList) ClientQueryManager.executeCreateAction(group.children,e.newTypeID,e.propField,e.theAction.name,Seq())
				listenerSet foreach (_.startActionDialog("",null,Nil,0)) 
				for(group<-groupList)ClientQueryManager.executeAction(group.parent,group.children,e.text,Seq())
			}
			else listenerSet foreach (_.startActionDialog(e.text,e.theAction.question.get,groupList,e.newTypeID))
		}	
	}  
}

