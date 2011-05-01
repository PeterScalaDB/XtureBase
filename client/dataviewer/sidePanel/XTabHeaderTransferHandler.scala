/**
 * Author: Peter Started:20.03.2011
 */
package client.dataviewer.sidePanel

import javax.swing._
import definition.data._
import definition.typ.AllClasses
import java.awt.datatransfer._
import java.io.Serializable
import client.comm.ClientQueryManager
import client.dataviewer.InstanceSelection

/**
 * 
 */
class XTabHeaderTransferHandler(controller:XTabSidePanelController) extends TransferHandler {

	override def getSourceActions(c:JComponent):Int = {  	    
  	   TransferHandler.NONE 
    }
	
	override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	val loc=support.getDropLocation
  	if( support.isDataFlavorSupported(InstanceSelection.flavor)) {
  			 val data = support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]  			 
  			 if(data.selection.size==0) return false  			 
  			 return true
  	} else return false
  }
	
	
	override def importData(info:TransferHandler.TransferSupport):Boolean  = {
    if (!info.isDrop()) 
      return false;    
   
    var data:InstanceSelection=null
    try {
    	data= info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
    } catch {
    	case e => System.out.println(e);return false
    }
    
    val fromOwner=new OwnerReference(data.propField.toByte,data.deserializedParents.first)
    for (dat <-data.deserializedSelection) try{
    	controller.tmodel .addColumn(dat,fromOwner)
    } catch {
    	case e => System.out.println(e);return false
    }   
    true
	}
}