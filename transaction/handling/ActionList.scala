/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._
import server.storage._
//import scala.collection.immutable.ListMap

/** manages the List of actions of a transaction
 * 
 * 
 */
object ActionList {
	var theList:Map[Reference,TransactionData]=Map.empty


	def breakAllData() = {
		println("Break")
	for(trans <- theList.values)
		(trans: @unchecked) match {
			case CreateAction(ref,_,_) => { // delete the Instances that got created during the try phase
				try {
					StorageManager.deleteInstance(ref.typ, ref.instance)
				} catch {case e => {println("BreakAllData error "+e)}} 
			}
	}
	reset()
}

def reset() = {theList=Map.empty}


def commitAllData() = {
	try {		
		for(trans <- theList.values)
			trans match {
				case CreateAction(ref,a,b) => { // Instances get created during the Try-Phase of the transaction
					for (data <-a)  StorageManager.writeInstance(data) // if instdata is defined, write
					for (pdata <-b)  StorageManager.writeInstanceProperties(pdata) // if propdata is defined, write
				}
				case DataChangeAction(in,pr,li) => {
					for(i <- in) StorageManager.writeInstance(i); // if instance is defined, write
					for(p <- pr) StorageManager.writeInstanceProperties(p) // if properties ...
					for(l <- li) StorageManager.writeReferencingLinks(l)
				}
				
				case DeleteAction(ref) => StorageManager.deleteInstance(ref.typ,ref.instance )
		}
		reset()
	} catch { case e:Exception => TransactionManager.breakTransaction() }
}


def addTransactionData (ref:Reference,newRec:TransactionData) = {
	println("add Transdata "+ref+"|"+newRec)
	if (theList.contains(ref))
		theList(ref) match { // is there already an transaction data for this instance ?			
			case a:CreateAction => newRec match { 
				// a createAction is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li ) => {
					if (in!=None) a.newInstData = in // add the new data to the createAction
					if (pr!=None) a.newPropData = pr
					if (li!=None) throw new IllegalArgumentException("Cant add external links to created Instance "+ref+" "+li)
				}				                                  
				case x: DeleteAction => throw new IllegalArgumentException("Delete after create for "+ref)
				case x: CreateAction =>  throw new IllegalArgumentException("Create after create for "+ref)
			}

			case a:DataChangeAction => newRec match {
				// a DataChange action is already there. What action shall be added ?
				case  DataChangeAction(in,pr,li ) => {
					if (in!=None) a.newInstData = in // add the new data to the createAction
					if (pr!=None) a.newPropData = pr
					if (li!=None) a.newLinksData = li
				}				
				case x: DeleteAction => theList += (ref ->newRec) // replace the datachange action with the delete
				case x: CreateAction =>  throw new IllegalArgumentException("Creating an existing instance "+ref)																	 
			}

			case a:DeleteAction => {} // drop the new action when the instance should already be deleted
		}
	else theList += (ref -> newRec)
}

/** checks if there is data  in the actionlist
 *  if yes, return it, else get the data from the DB
 */
def getInstanceData(ref:Reference):InstanceData = {
	if (theList.contains(ref))
		(theList(ref): @unchecked)  match {
		case CreateAction(_,Some(data),_) => return data
		case DataChangeAction(Some(data),_,_) => return data
		case _ => 
	}
	StorageManager.getInstanceData(ref)
}


def getInstanceProperties(ref:Reference):Option[InstanceProperties] = {
	if (theList.contains(ref))
		(theList(ref): @unchecked) match {
		case CreateAction(_,_,a:Some[InstanceProperties]) => return a
		case DataChangeAction(_,a:Some[InstanceProperties],_) => return a
		case _ => // ignore
	}
	StorageManager.getInstanceProperties(ref)
}

/** checks if there are data for referencing links in the actionlist
 *  if yes, return them, else get the data from the DB
 */
def getReferencingLinks(ref:Reference):Option[ReferencingLinks] = {
	if(theList.contains(ref))
		(theList(ref): @unchecked) match {
		   case DataChangeAction(_,_,a:Some[ReferencingLinks]) => return a
		   case _ => 
	  }
	StorageManager.getReferencingLinks(ref)
}


}