/**
 * Author: Peter Started:13.11.2010
 */
package client.dataviewer

import javax.swing._
import definition.data._
import java.awt.datatransfer._
import java.io.Serializable
import client.comm.ClientQueryManager
import definition.expression.FieldReference


/**
 * 
 */
class TableTransferHandler(tableMod:TypeTableModel) extends TransferHandler {
	
   override def getSourceActions(c:JComponent):Int = {  	    
  	   //System.out.println("getSourceActions ")
  	   if(tableMod.selectedInstances .size==1) TransferHandler.COPY_OR_MOVE +TransferHandler.LINK
  	   else  TransferHandler.COPY_OR_MOVE 
    }
   override def createTransferable(c:JComponent ):InstanceSelection = {
  	 val ni=  new InstanceSelection(Array(tableMod.getParentRef),tableMod.getPropField,tableMod.selectedInstances,
  		 tableMod.selectedInstances .map (_.toString).toArray,tableMod.table.peer.getSelectedColumn(),
  		 tableMod.table .peer.getSelectedRows())
  	 //System.out.println(ni)
  	 ni
   }
   
   override def exportDone(source:JComponent,data: Transferable,action:Int):Unit = {
  	 action match {
  		 case TransferHandler.COPY => System.out.println("Source Copied")
  		 case TransferHandler.MOVE => System.out.println("Source Moved")
  		 case TransferHandler.LINK => System.out.println("Source Linked")
  		 case TransferHandler.NONE =>  
  	 }
   }
   
   override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	 //System.out.println("tth can Import loc: "+support.getDropLocation+ " da:" + support.getDropAction+" sda: "+support.getSourceDropActions+" "+support.isDrop)
  	 if(support.getDropLocation.isInstanceOf[JTable.DropLocation])
  	 {
  		 val tabLoc=support.getDropLocation.asInstanceOf[JTable.DropLocation]  		  
  		 if( support.isDataFlavorSupported(InstanceSelection.flavor)) {
  			 val data = support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
  			 if(data.selection.size==0) return false
  			 if(tableMod.propMod.getPropFieldDefinition.single&&( !tableMod.propMod.isEmpty|| data.selection.size!=1)) return false
  			 support.getDropAction match {
  				 case TransferHandler.LINK => {
  					 if( tabLoc.getRow>=tableMod.dataList.size) return false
  					 if(data.selection.size!=1) return false
  					 val thisInst=tableMod.dataList(tabLoc.getRow)
  					 if(data.selection.first equals thisInst.ref) {
  						 return data.dragColumn!=tabLoc.getColumn
  					 }
  					 return true
  				 }
  				 case TransferHandler.MOVE | TransferHandler.COPY => {
  				   if (data.selection.first.typ!=tableMod.typ) return false
  				   if( (data.parentRefs.first equals tableMod.getParentRef) &&
  				  		 data.dragRows.contains(tabLoc.getRow)){
  				  	 //System.out.println("same same")
  				  	 return false // same parent
  				   }
  				   return true	 
  				 }
  				 case _ => false
  			 }
  			 //System.out.println(" loc:"+tabLoc.getRow+ " data.parent:"+data.parentRefs .first + " "+tableMod.getParentRef)  			 	 
  		 } else false
  	 } else false    	 
   }
   
  override def importData(info:TransferHandler.TransferSupport):Boolean  = {
    if (!info.isDrop()) 
      return false;
    
    val tabLoc=info.getDropLocation.asInstanceOf[JTable.DropLocation]
    val action= info.getDropAction
    val row=tabLoc.getRow
    var data:InstanceSelection=null
    try {
    	data= info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
    } catch {
    	case e => System.out.println(e);return false
    }
    //System.out.println(" row:"+row+" insert:"+tabLoc.isInsertRow)
    action match {
    	case TransferHandler.COPY =>{
    		System.out.println("Copied")
    		//val wishRow=if(row>=tableMod.dataList.size) tableMod.dataList.size-1 else row
    		tableMod.wishSelection=(row until row+data.selection .size)
    		ClientQueryManager.runInPool {
    			ClientQueryManager.copyInstances(data.deserializedSelection,
    				new OwnerReference(data.propField.toByte,data.deserializedParents.first),
    				new OwnerReference(tableMod.getPropField,tableMod.getParentRef),row)
    		}
    	}
  		case TransferHandler.MOVE => {
  			System.out.println("Moved")
  			val fromOwner=new OwnerReference(data.propField.toByte,data.deserializedParents.first)
  			val toOwner=new OwnerReference(tableMod.getPropField,tableMod.getParentRef)
  			val wishRow=if(fromOwner==toOwner && row>=tableMod.dataList.size) tableMod.dataList.size-1 else row
    		tableMod.wishSelection=(wishRow until (wishRow+data.selection .size))
    		ClientQueryManager.runInPool {
  			ClientQueryManager.moveInstances(data.deserializedSelection,fromOwner,toOwner,row)}
  		}
  		case TransferHandler.LINK => {
  			if( tabLoc.getRow>=tableMod.dataList.size) return false
  			if(data.selection.size!=1) return false
  			val thatRef=data.selection.first.toReference
  			val thisInst=tableMod.dataList(tabLoc.getRow)
  			val remType=if(thisInst.ref.typ==thatRef.typ)None else Some(thatRef.typ)
  			val remInst=if(!remType.isDefined && thisInst.ref.instance==thatRef.instance)None else Some(thatRef.instance)
  			if((!remType.isDefined) && (!remInst.isDefined)&& data.dragColumn == tabLoc.getColumn) {
  				System.out.println(" cant link to same field "+thatRef+" "+tabLoc.getColumn)
  				return false
  			}
  			System.out.println("link from "+thatRef+" field:"+data.dragColumn+" toField:"+tabLoc.getColumn)
  			ClientQueryManager.writeInstanceField(thisInst.ref, colToMod(tabLoc.getColumn),
  				new FieldReference(remType,remInst,colToMod(data.dragColumn)))
  		}
  		case TransferHandler.NONE =>  
    }
    true
  }
  def colToMod(col:Int):Byte = {
  	(tableMod.table.peer.convertColumnIndexToModel(col)-1).toByte
  }
}

@SerialVersionUID(4276L) 
class InstanceSelection extends Transferable with Serializable {
	//private val serialVersionUID = 4275L
	
	def this(parents:Array[Referencable],npropField:Int,selChildren:Seq[Referencable],texts:Array[String],
	         ncolumn:Int,nrow:Array[Int]) = {
		this()
		parentRefs=parents.map(_.ref.serialized)
		selection=selChildren.map(_.ref.serialized).toArray
		textArray=texts
		dragColumn=ncolumn
		propField=npropField
		dragRows=nrow
		//System.out.println(" create Sel :"+dragRows.mkString(","))
	}
	
	var dragColumn:Int=0
	var dragRows:Array[Int]=Array()
	var parentRefs:Array[SerialReference]=Array()
	var propField:Int=0
	var selection:Array[SerialReference]=Array()
	var textArray:Array[String]=Array()
	
	def getTransferData(flavor:DataFlavor ) = {
		//System.out.println("get Transfer Data "+flavor)
		if(flavor.equals(InstanceSelection.flavor)) this
		else if (flavor.equals(InstanceSelection.flavors(1))) 
	     toString	 
	 else  throw new UnsupportedFlavorException(flavor);
	}		
	
	
	def getTransferDataFlavors()= {
		InstanceSelection.flavors
	}
	
	def deserializedSelection:Seq[Reference]= selection.map(_.toReference).toSeq
	def deserializedParents:Seq[Reference] =  parentRefs.map(_.toReference).toSeq
	
	def isDataFlavorSupported(flavor:DataFlavor):Boolean = {
		for( f <-InstanceSelection.flavors) 
			if(flavor equals f) return true
		return false
	}
	
	override def toString="InstSel parents:"+parentRefs.mkString+"selection:"+selection.mkString+"\ntextArray:"+(  textArray mkString "\n")+
	 " dragColumn:"+dragColumn+" dragrows:"+dragRows.mkString(",")+" propField:"+propField
}


object InstanceSelection {
	val flavor= new DataFlavor(classOf[InstanceSelection],"DatabaseObject")
	
	val flavors= Array(flavor,DataFlavor.stringFlavor)
}