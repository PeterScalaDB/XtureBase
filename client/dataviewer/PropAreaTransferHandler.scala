/**
 * Author: Peter Started:14.11.2010
 */
package client.dataviewer

import javax.swing._
import definition.data._
import definition.typ.AllClasses
import java.awt.datatransfer._
import java.io.Serializable
import client.comm.ClientQueryManager
/**
 * 
 */
class PropAreaTransferHandler(propMod:PropertyModel) extends TransferHandler{
	def propField=propMod.getPropFieldDefinition
	
	override def getSourceActions(c:JComponent):Int = {  	    
  	   TransferHandler.NONE 
    }
	
  override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	val loc=support.getDropLocation
  	if( support.isDataFlavorSupported(InstanceSelection.flavor)) {
  			 val data = support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
  			 //System.out.println("allowedClass: "+propField.allowedClass +" data class:"+data.selection .first.typ+" sel:"+data.selection.size+" single:"+propField.single+
  			 //	 " empty:"+propMod.isEmpty )
  			 if(data.selection.size==0) return false
  			 if(propField.single&&( !propMod.isEmpty|| data.selection.size!=1)) return false
  			 
  			 if(propField.allowedClass>0 && !AllClasses.get.getClassByID(data.selection.first.typ).inheritsFrom(  propField.allowedClass)) return false
  			 //if(data.parentRefs .first equals propMod.mainController.ref) return false
  			 support.getDropAction match {
  				 case TransferHandler.LINK => return true
  				 case TransferHandler.COPY | TransferHandler.MOVE => return true
  				 case _ => return false
  			 }
  	} else return false
  }
  
  override def importData(info:TransferHandler.TransferSupport):Boolean  = {
    if (!info.isDrop()) 
      return false;
    
    //val tabLoc=info.getDropLocation.asInstanceOf[JTable.DropLocation]
    val action= info.getDropAction    
    var data:InstanceSelection=null
    try {
    	data= info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
    } catch {
    	case e => System.out.println(e);return false
    }
     action match {
    	case TransferHandler.COPY =>{
    		System.out.println("Copied")
    		ClientQueryManager.copyInstances(data.deserializedSelection,
    			new OwnerReference(data.propField.toByte,data.deserializedParents.first),
    			propMod.ownerRef,-1)
    	}
  		case TransferHandler.MOVE => {
  			System.out.println("Moved")
  			ClientQueryManager.moveInstances(data.deserializedSelection,
    			new OwnerReference(data.propField.toByte,data.deserializedParents.first),
    			propMod.ownerRef,-1)
  		}
  		case TransferHandler.LINK => {
  			System.out.println("Linked")
  			ClientQueryManager.secondUseInstances(data.deserializedSelection,
    			new OwnerReference(data.propField.toByte,data.deserializedParents.first),
    			propMod.ownerRef,-1)
  		}
  		case _ => false
     }
    true
  }
}