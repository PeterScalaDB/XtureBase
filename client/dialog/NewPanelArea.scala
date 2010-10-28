/**
 * Author: Peter Started:27.10.2010
 */
package client.dialog
import java.awt.{KeyboardFocusManager,Dimension}
import java.beans._ 

import scala.swing.{BorderPanel,BoxPanel,Label}
import javax.swing._
import javax.swing.border._

/** area where "New panels" are placed
 * 
 */
class NewPanelArea extends BorderPanel {
  
	val IGNOREVALUE=new scala.swing.Component {}
	
	val label=new Label("Neue Objekte:")
	label.preferredSize=new Dimension(50,35)
	
	val childPanel= new BoxPanel(scala.swing.Orientation.Vertical)
	childPanel.border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
	
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
     })
  
  def findNewPanel(comp:javax.swing.JComponent):Option[scala.swing.Component]= {
  	if(comp==null) return None
  	val newPan=comp.getClientProperty("newPanel")
  	if(newPan!=null)
  		if(newPan.isInstanceOf[scala.swing.Component])
  			return Some(newPan.asInstanceOf[scala.swing.Component])
  		else {
  			//println("Ignore focus !!!")
  			return Some(IGNOREVALUE)
  		}
  	if(!comp.getParent.isInstanceOf[JComponent])return None
  	else findNewPanel(comp.getParent.asInstanceOf[JComponent])
  }
}