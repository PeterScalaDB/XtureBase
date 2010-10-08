/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._
import server.storage._
import server.test.SimpleProfiler
import server.comm.CommonSubscriptionHandler
//import scala.collection.immutable.ListMap

/** manages the List of actions of a transaction
 * 
 * 
 */
object ActionList {
	var theList=collection.mutable.LinkedHashMap[Reference,TransactionData]()


	def breakAllData() = {
		println("Break")
	for(trans <- theList.valuesIterator)
		trans match {
			case CreateAction(ref,_,_,_) => { // delete the Instances that got created during the try phase
				try {
					StorageManager.deleteInstance(ref.typ, ref.instance)
				} catch {case e => {println("BreakAllData error "+e)}} 
			}
			case _ =>
	}
	reset()
}

def reset() = {theList.clear}


def commitAllData() = {
	try {		
		//println(theList)
		for(trans <- theList.valuesIterator)
			trans match {
				case CreateAction(ref,a,b,co) => { // Instances get created during the Try-Phase of the transaction
					for (pdata <-b){
						StorageManager.writeInstanceProperties(pdata) // if propdata is defined, write						
					}
					for (data <-a) {
						StorageManager.writeInstance(data,true) // if instdata is defined, write
						val sendData =if(b.isDefined && b.get.hasChildren !=data.hasChildren) data.setHasChildren(b.get.hasChildren)
						   else data
						for(owner <-data.owners)
							CommonSubscriptionHandler.instanceCreated(owner,sendData)
					}
					
					for (c <- co) StorageManager.writeCollectingFuncData(c)
				}
				case DataChangeAction(in,pr,li,co) => {
					for(i <- in){
						StorageManager.writeInstance(i,false); // if instance is defined, write
						val sendData =if(pr.isDefined && pr.get.hasChildren !=i.hasChildren) i.setHasChildren(pr.get.hasChildren)
						   else i
						CommonSubscriptionHandler.instanceChanged(sendData)
					}
					for(p <- pr) StorageManager.writeInstanceProperties(p) // if properties ...
					for(l <- li) StorageManager.writeReferencingLinks(l)
					for(c <- co) StorageManager.writeCollectingFuncData(c)
				}
				
				case DeleteAction(inst) => {
					StorageManager.deleteInstance(inst.ref.typ,inst.ref.instance )
					for(owner <-inst.owners)
					CommonSubscriptionHandler.instanceDeleted(owner,inst.ref)
				}
				//TODO: delete link, property and collFunc data !
		}
		//SimpleProfiler.finish("commit "+theList.size)
		reset()
	} catch { case e:Exception => TransactionManager.breakTransaction() }
}


def addTransactionData (ref:Reference,newRec:TransactionData) = {
	//println("add Transdata "+ref+"|"+newRec)
	if (theList.contains(ref))
		theList(ref) match { // is there already an transaction data for this instance ?			
			case a:CreateAction => newRec match { 
				// a createAction is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li,co ) => {
					if (in!=None) a.newInstData = in // add the new data to the createAction
					if (pr!=None) a.newPropData = pr
					if (li!=None) throw new IllegalArgumentException("Cant add external links to created Instance "+ref+" "+li)
					if (co!=None) a.newCollData= co
				}				                                  
				case x: DeleteAction => throw new IllegalArgumentException("Delete after create for "+ref)
				case x: CreateAction =>  throw new IllegalArgumentException("Create after create for "+ref)
			}

			case b:DataChangeAction => newRec match {
				// a DataChange action is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li,co ) => {
					if (in!=None) b.newInstData = in // add the new data to the createAction
					if (pr!=None) b.newPropData = pr
					if (li!=None) b.newLinksData = li
					if (co!=None) b.newCollData= co
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
		case CreateAction(_,Some(data),_,_) => return data
		case DataChangeAction(Some(data),_,_,_) => return data
		case a:DeleteAction => return null 
		case _ => // drink another beer
	}
	StorageManager.getInstanceData(ref)
}


def getInstanceProperties(ref:Reference):Option[InstanceProperties] = {
	if (theList.contains(ref))
		(theList(ref)) match {
		case CreateAction(_,_, a @ Some(_),_) => return a
		case DataChangeAction(_,a @ Some(_),_,_) => return a
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
		   case DataChangeAction(_,_,a@ Some(_),_) => return a
		   case _ => 
	  }
	StorageManager.getReferencingLinks(ref)
}

def getCollData(ref:Reference):Option[CollFuncResultSet] =
{
	if (theList.contains(ref))
		(theList(ref)) match {
		case CreateAction(_,_,_,a@ Some(_)) => return a
		case DataChangeAction(_,_,_,b @ Some(_)) => return b
		case _ => // ignore
	}
	StorageManager.getCollectingFuncData(ref)
}


}