/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._
import server.storage._
//import server.test.SimpleProfiler
import server.comm.CommonSubscriptionHandler
//import scala.collection.immutable.ListMap

/** manages the List of actions of a transaction
 * 
 * 
 */
object ActionList {
	var theList=collection.mutable.LinkedHashMap[Reference,TransactionData]()
  
	val copiedInstances=collection.mutable.HashMap[Reference,Reference]()
  
	def isEmpty=theList.isEmpty
	
	def breakAllData() = {
		System.out.println("Break")
	for(trans <- theList.valuesIterator)
		trans match {
			case CreateAction(ref,_,_,_,_,_) => { // delete the Instances that got created during the try phase
				try {
					StorageManager.deleteInstance(ref.typ, ref.instance)
				} catch {case e => {System.out.println("BreakAllData error "+e)}} 
			}
			case _ =>
	}
	reset()
}

def reset() = {
		theList.clear
		copiedInstances.clear
	}

def notifyInstanceCopied(oldRef:Reference,copyRef:Reference):Unit = copiedInstances(oldRef)=copyRef

def isInstanceCopied(ref:Reference):Boolean= copiedInstances.contains(ref)

def getCopyForRef(oldRef:Reference)=copiedInstances(oldRef)


def commitAllData() = {
	var hasMoveOrCopy:Boolean=false
	try {		
		//System.out.println("actions:\n"+theList.mkString("\n"))
		for(trans <- theList.valuesIterator)
			trans match {
				case CreateAction(ref,instData,propData,linkData,collData,cmi) => { // Instances get created during the Try-Phase of the transaction
					for (pdata <-propData){
						StorageManager.writeInstanceProperties(pdata) // if propdata is defined, write						
					}
					for (data <-instData) {
						StorageManager.writeInstance(data,true) // if instdata is defined, write
						val sendData =if(propData.isDefined && propData.get.hasChildren !=data.hasChildren) data.setHasChildren(propData.get.hasChildren)
						   else data
						cmi match {
						  	 case Some(AddDontNotifyOwners) => {}
						  	 case _ => for(owner <-data.owners) // in all other cases notify owners
						  		 						CommonSubscriptionHandler.instanceCreated(owner,sendData)
						   }
						//if(!cmi.isDefined || ! cmi.get.isInstanceOf[AddDontNotifyOwners.type] )							
					}
					
					for (c <- collData) StorageManager.writeCollectingFuncData(c)
					for(l <- linkData) StorageManager.writeReferencingLinks(l)					
				}
				case DataChangeAction(instData,propData,linkData,collData,cmi,deleteFromUser) => {
					for(i <- instData){
						StorageManager.writeInstance(i,false); // if instance is defined, write
						val sendData =if(propData.isDefined && propData.get.hasChildren !=i.hasChildren) i.setHasChildren(propData.get.hasChildren)
						   else i
						cmi match {
						  	 case Some(ChangeDontNotifyOwners) => {}
						  	 case _ => CommonSubscriptionHandler.instanceChanged(sendData) 
						   }   
						
					}
					for(p <- propData){
						StorageManager.writeInstanceProperties(p) // if properties ...	
						 // child was copied or moved here
						cmi match {
							case Some(RefreshDestinationOwner) => hasMoveOrCopy=true
							case Some(RemoveNotifySourceOwners(fromOwner,childList))=> 
							for(child<-childList) CommonSubscriptionHandler.instanceDeleted(fromOwner,child)
							case _ =>
						}
							
					}
					for(l <- linkData) StorageManager.writeReferencingLinks(l)
					for(c <- collData) StorageManager.writeCollectingFuncData(c)
					for(d <- deleteFromUser) CommonSubscriptionHandler.instanceDeleted(d,instData.get.ref)
				}
				
				case DeleteAction(inst) => {
					 StorageManager.deleteInstance(inst.ref.typ,inst.ref.instance )
					for(owner <-inst.owners)
						CommonSubscriptionHandler.instanceDeleted(owner,inst.ref)
					for(owner <-inst.secondUseOwners)
						CommonSubscriptionHandler.instanceDeleted(owner,inst.ref)
				}
				//TODO: delete link, property and collFunc data !
		}
		//CommonSubscriptionHandler.submitBurstInfo()
		// notify property changes for move and copy
		if(hasMoveOrCopy ) {
			for(trans <- theList.valuesIterator)
			 trans match {
				case DataChangeAction(_,Some(pr),_,_,Some(RefreshDestinationOwner),_) => {
					CommonSubscriptionHandler.refreshSubscriptionsFor(pr.ref)
				}
				case _ =>
			}
		}
		
		//SimpleProfiler.finish("commit "+theList.size)
		reset()
	} catch { case e:Exception => TransactionManager.breakTransaction() }
}


def addTransactionData (ref:Reference,newRec:TransactionData) = {
	//System.out.println("add Transdata "+ref+"|"+newRec)
	if (theList.contains(ref))
		theList(ref) match { // is there already an transaction data for this instance ?			
			case a:CreateAction => newRec match { 
				// a createAction is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li,co,cmi,_ ) => {
					if (in!=None) a.newInstData = in // add the new data to the createAction
					if (pr!=None) a.newPropData = pr
					if (li!=None)/* if(a.newLinksData!=None) throw new IllegalArgumentException("Cant add external links to created Instance "+ref+" "+li+" oldlinks:"+a.newLinksData )
					else*/ a.newLinksData =li
					if (co!=None) a.newCollData= co	
					if(cmi.isDefined) a.cmi=if(a.cmi.isDefined) Some(a.cmi.get.replaceWith(cmi.get)) else cmi
					//if(pos != -1) a.atPosition=pos
				}				                                  
				case x: DeleteAction => throw new IllegalArgumentException("Delete after create for "+ref)
				case x: CreateAction =>  throw new IllegalArgumentException("Create after create for "+ref)
			}

			case b:DataChangeAction => newRec match {
				// a DataChange action is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li,co,cmi,deFrOw) => {
					if (in!=None) b.newInstData = in // add the new data to the createAction
					if (pr!=None) b.newPropData = pr
					if (li!=None) b.newLinksData = li
					if (co!=None) b.newCollData= co
					if(cmi.isDefined) b.cmi=if(b.cmi.isDefined) Some(b.cmi.get.replaceWith(cmi.get)) else cmi
					if(deFrOw!=None) b.deleteFromOwner=deFrOw
				}				
				case x: DeleteAction => theList += (ref ->newRec) // replace the datachange action with the delete
				case x: CreateAction =>  throw new IllegalArgumentException("Creating an existing instance "+ref)																	 
			}

			case a:DeleteAction => {} // drop the new action when the instance should already be deleted
		}
	// no data for that ref yet, add the new TransactionData
	else theList += (ref -> newRec)
}

/** checks if there is data  in the actionlist
 *  if yes, return it, else get the data from the DB
 */
def getInstanceData(ref:Reference):InstanceData = {
	if (theList.contains(ref))
		(theList(ref))  match {
		case CreateAction(_,Some(data),_,_,_,_) => return data
		case DataChangeAction(Some(data),_,_,_,_,_) => return data
		case a:DeleteAction => return null 
		case _ => // drink another beer
	}
	StorageManager.getInstanceData(ref)
}


def getInstanceProperties(ref:Reference):Option[InstanceProperties] = {
	if (theList.contains(ref))
		(theList(ref)) match {
		case CreateAction(_,_, a @ Some(_),_,_,_) => return a
		case DataChangeAction(_,a @ Some(_),_,_,_,_) => return a
		case _ => // ignore
	}
	StorageManager.getInstanceProperties(ref)
}

/** checks if there are data for referencing links in the actionlist
 *  if yes, return them, else get the data from the DB
 */
def getReferencingLinks(ref:Reference):Option[ReferencingLinks] = {
	if(theList.contains(ref))
		(theList(ref)) match {
			 case CreateAction(_,_,_, a @ Some(_),_,_) =>{System.out.println("get links hit create"); return a}
		   case DataChangeAction(_,_,a@ Some(_),_,_,_) => {System.out.println("get links hit change"); return a}
		   case _ => 
	  }
	StorageManager.getReferencingLinks(ref)
}

def getCollData(ref:Reference):Option[CollFuncResultSet] =
{
	if (theList.contains(ref))
		(theList(ref)) match {
		case CreateAction(_,_,_,_,a@ Some(_),_) => return a
		case DataChangeAction(_,_,_,b @ Some(_),_,_) => return b
		case _ => // ignore
	}
	StorageManager.getCollectingFuncData(ref)
}


}