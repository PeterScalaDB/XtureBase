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
												var newPropData:Option[InstanceProperties]=None //optional property data to be
) extends TransactionData

case class DataChangeAction(var newInstData:Option[InstanceData],
														var newPropData:Option[InstanceProperties]=None,
														var newLinksData:Option[ReferencingLinks]=None) extends TransactionData

case class DeleteAction(ref:Reference) extends TransactionData

