/**
 * Author: Peter Started:27.10.2010
 */
package client.dialog
import java.awt.{KeyboardFocusManager,Dimension}
import java.beans._ 

import scala.swing.{BorderPanel,BoxPanel,Label}
import javax.swing._
import javax.swing.border._
import definition.typ._
import definition.expression.Constant
import definition.data.{Reference,Referencable}

/** area where "New panels" are placed
 * 
 */



class NewPanelArea extends AbstractActionPanel with ContainerFocusListener {
  var lastContainer:String=null
  var lastSuperInstRef:Reference=null
  var lastPropField:Int=0  
	
	
	def containerFocused(superInst:Referencable, propField:Int,containerName:String=""):Unit = {
  	//println("Container focused  superInstRef:"+superInst.ref+" propField:"+propField)
  	if(superInst.ref!=lastSuperInstRef){ 
  		instList= List(superInst)
  		lastSuperInstRef=superInst.ref
  	}
		if(containerName==lastContainer&&superInst.ref.typ==lastSuperInstRef.typ&&propField==lastPropField) return
		// Create Action set changed:		
		//if(superInst==null )
		shutDown
		val theClass=AllClasses.get.getClassByID(superInst.ref.typ)
		val propF=theClass.propFields(propField)
		if(!propF.createChildDefs.isEmpty) {			
		  for(childDef <-propF.createChildDefs;if (childDef.editorName==containerName)) {
		  	val b = if(childDef.actionName =="*") getButton(dummyAction,childDef.childClassName)
		  	else getButton(childDef.getAction,"")
		  	b.newTypeID=childDef.getChildTyp
		  	b.propField=propField.toByte
		  	contents+=b
		  }
				
		}			
		lastContainer=containerName
		lastPropField=propField
		revalidate
		repaint
	}
  
  val dummyAction=new ActionDescription("*",None,true)
 
	
	
	
	/*val IGNOREVALUE=new scala.swing.Component with NewPanelTrait { 
		def setNewPanelArea(area:NewPanelArea)= {}
		def sendResults(resultList:Seq[(String,Constant)])= {}
	}*/
	
	/*val label=new Label("Neue Objekte:")
	label.preferredSize=new Dimension(50,35)
	
	val childPanel= new BoxPanel(scala.swing.Orientation.Vertical)
	childPanel.border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
	
	var listenerSet=collection.mutable.HashSet[ActionPanListener]()
	var currentNewPanel:NewPanelTrait =null
	
	add (label,BorderPanel.Position.North)
	add (childPanel,BorderPanel.Position.Center)
	
	val kbm=  KeyboardFocusManager.getCurrentKeyboardFocusManager();
  kbm.addPropertyChangeListener(
     new PropertyChangeListener() {
       def propertyChange(e:PropertyChangeEvent):Unit=
         if("focusOwner"==e.getPropertyName)
         {
        	 if(e.getNewValue!=null)
        	 findNewPanel(e.getNewValue.asInstanceOf[javax.swing.JComponent]) match {
        		 case Some(IGNOREVALUE) => { // ignore 
        			 //println(" ignore")
        		 }
        		 case Some(newP) => { // Panel provider found        			 
        			 //println("panel provider newpan:"+newP)
        			 if(childPanel.contents.size==1) {
        				 if(childPanel.contents.head==newP) return // ignore if the new Panel is already shown  
        			 }
        			 childPanel.contents.clear()
        			 childPanel.contents+=newP
        			 newP.setNewPanelArea(NewPanelArea.this)
        			 currentNewPanel=newP
        			 revalidate
        			 repaint
        		 }
        		 case None => {
        			 //println("focus none")
        			 if(childPanel.contents.size>0)
        			 {
        				 childPanel.contents.clear()
        				 revalidate
        				 repaint
        			 }        			 
        		 }
        	 }
         }
     })*/
  
  /*def findNewPanel(comp:javax.swing.JComponent):Option[scala.swing.Component with NewPanelTrait]= {
  	if(comp==null) return None
  	val newPan=comp.getClientProperty("newPanel")
  	if(newPan!=null)
  		if(newPan.isInstanceOf[scala.swing.Component])
  			return Some(newPan.asInstanceOf[scala.swing.Component with NewPanelTrait])
  		else {
  			//println("Ignore focus !!!")
  			return Some(IGNOREVALUE)
  		}
  	if(!comp.getParent.isInstanceOf[JComponent])return None
  	else findNewPanel(comp.getParent.asInstanceOf[JComponent])
  }
  
  def registerNewPanListener(listener:ActionPanListener) = {
		listenerSet+=listener
	}
  
  def notifyListeners(text:String,question:ParamQuestion)= {
  	//if(currentNewPanel!=null)
    //listenerSet foreach (_.startNewDialog(text,question,currentNewPanel.sendResults	))	
    
  }*/
  
}