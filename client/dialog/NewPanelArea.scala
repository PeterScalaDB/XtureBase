/**
 * Author: Peter Started:27.10.2010
 */
package client.dialog
import java.awt.{KeyboardFocusManager,Dimension}
import java.beans._ 

import scala.swing.{BorderPanel,BoxPanel,Label}
import scala.swing.event._
import javax.swing._
import javax.swing.border._
import definition.typ._
import definition.expression.Constant
import definition.data.{Reference,Referencable}
import client.comm.ClientQueryManager

/** area where "New panels" are placed
 * 
 */



object NewPanelArea extends AbstractActionPanel with ContainerFocusListener {
  private var lastContainer:String=null
  private var lastSuperInstRef:Reference=null
  private var lastPropField:Int= -1  
	private var selGroup=new SelectGroup[Referencable](null,Seq[Referencable]())
	
	def containerFocused(superInst:Referencable, propField:Int,containerName:String=""):Unit = {  	
  	//System.out.println("lastContainer:"+lastContainer+" lastSuperInst:"+lastSuperInstRef+")
  	if(containerName==lastContainer&&superInst.ref==lastSuperInstRef&&propField==lastPropField) return  	
  	//System.out.println("Container focused  superInstRef:"+superInst.ref+" propField:"+propField+" lastSuperInst:"+lastSuperInstRef)		
		// Create Action set changed:		
		//if(superInst==null )
		//System.out.println("loading")
  	if(lastSuperInstRef==null || superInst.ref.typ!=lastSuperInstRef.typ||lastPropField!=propField||containerName!=lastContainer) {
  		shutDown
  		val theClass=AllClasses.get.getClassByID(superInst.ref.typ)
  		val propF=theClass.propFields(propField)
  		//System.out.println("setup class:"+theClass.name)
  		if(!propF.createChildDefs.isEmpty) {			
  			for(childDef <-propF.createChildDefs;if (childDef.editorName==containerName)) {
  				val b = if(childDef.actionName =="*") getButton(dummyAction,childDef.childName)//childDef.childClassName
  				else getButton(childDef.action,"")
  				//System.out.println("childDef:"+childDef)
  				b.newTypeID=childDef.childClassID
  				b.propField=propField.toByte
  				contents+=b
  			}
  		}
  		revalidate
  		repaint
  		lastContainer=containerName
  		lastPropField=propField
  	}
  	if(superInst.ref!=lastSuperInstRef){
  		selGroup.children= List(superInst)
  		lastSuperInstRef=superInst.ref
  	}
				
	}
  
  val dummyAction=new ActionDescription("*",None,true)	
  
  reactions += {
		case ButtonClicked(e:ActionButton) => if(!selGroup.children.isEmpty){			
			if (e.theAction.question ==None ) {
				//System.out.println("Execute action " +e.text)
				if(e.newTypeID>0) {
					listenerSet foreach (_.startActionDialog("",null,Nil,0))
					ClientQueryManager.executeCreateAction(selGroup.children,e.newTypeID,e.propField,e.theAction.name,Seq())
				}
				// for(group<-groupList)ClientQueryManager.executeAction(group.parent,group.children,e.text,Seq())
			}
			else listenerSet foreach (_.startActionDialog(e.text,e.theAction.question.get,List(selGroup),e.newTypeID))
		}	
	}  
  
	
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
        			 //System.out.println(" ignore")
        		 }
        		 case Some(newP) => { // Panel provider found        			 
        			 //System.out.println("panel provider newpan:"+newP)
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
        			 //System.out.println("focus none")
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
  			//System.out.println("Ignore focus !!!")
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