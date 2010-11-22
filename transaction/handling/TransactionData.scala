/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._

/** Superclass for the different Transaction Types
 * 
 */
sealed abstract class TransactionData {
  
}

case class CreateAction(ref:Reference, // reference of the new Instance
												var newInstData:Option[InstanceData]=None, // optional data to be stored in the new instance
												var newPropData:Option[InstanceProperties]=None, //optional property data to be
												var newLinksData:Option[ReferencingLinks]=None,
												var newCollData:Option[CollFuncResultSet]=None,
														var cmi:Option[CopyMoveInfo]=None												 
) extends TransactionData

case class DataChangeAction(var newInstData:Option[InstanceData],
														var newPropData:Option[InstanceProperties]=None,
														var newLinksData:Option[ReferencingLinks]=None,
														var newCollData:Option[CollFuncResultSet]=None,
														var cmi:Option[CopyMoveInfo]=None		) extends TransactionData // is called by copy to notify

case class DeleteAction(inst:InstanceData) extends TransactionData

// special transaction information for copy and move transactions
trait CopyMoveInfo {
	def replaceWith(other:CopyMoveInfo):CopyMoveInfo = {
		other
	}
}

// force all Subscriptions to the destination owner to refresh their data
object RefreshDestinationOwner extends CopyMoveInfo{
	val some=Some(this)	 
	override def replaceWith(other:CopyMoveInfo):CopyMoveInfo = {
		this
	}
}

// when a copy instance is added to an owner, dont notify destination owners - they will be refreshed
object AddDontNotifyOwners extends CopyMoveInfo {
	val some=Some(this)
}

// when a copy instance is added to an owner, dont notify destination owners - they will be refreshed
object ChangeDontNotifyOwners extends CopyMoveInfo {
	val some=Some(this)
}


case class RemoveNotifySourceOwners(owner:OwnerReference,var childRefs:List[Reference]) extends CopyMoveInfo {
	override def replaceWith(other:CopyMoveInfo):CopyMoveInfo = {
		//println(toString + " replace with "+other)
		other match {
			case RefreshDestinationOwner   => other			
			case AddDontNotifyOwners|ChangeDontNotifyOwners => this
			case RemoveNotifySourceOwners(oOwner,oChildRefs) => {
				if(oOwner!=owner) throw new IllegalArgumentException("RemoveNotifySourceOwners different owners:"+owner +" "+oOwner)
				childRefs= childRefs ++ oChildRefs
				this
			}			
		}
		
	}
}