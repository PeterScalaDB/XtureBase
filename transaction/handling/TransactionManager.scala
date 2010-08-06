/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import server.storage._
import server.config._
import definition.typ._
import definition.data._
import definition.expression._

/** manages Transaction handling
 * 
 */
object TransactionManager {
  var running=false
	
	
	// Initalisation of the Storage system
  def init() = {
  	AllClasses.fromXML( xml.XML.loadFile(FSPaths.configDir+"types.xml" ))
  	StorageManager.init(AllClasses.getClassList)
  	println("Max Trans:"+TransLogHandler.transID)
  }
			
	
	
	// Starts a new Transaction
	private def startTransaction() =	{		
	  if(running ) throw new IllegalArgumentException("An Transaction is still running ")
		running=true
	  TransLogHandler.incrementTransID();
	  println("Start Trans " +TransLogHandler.transID)
	}
	
	// Finishes the Transaction and commits all changes to the database
	private def finishTransaction() = 	{
		
		if(!running) throw new IllegalArgumentException("Finish: No transaction running ")		
		ActionList.commitAllData()
		println("Finish Trans "+ TransLogHandler.transID)
		running=false
	}
	
	
	// breaks an transaction if an error occurs during the transaction
	 def breakTransaction() = 	{
		ActionList.breakAllData()		
		TransLogHandler.resetTransID()
		running=false
	}
	
	
	def tryCreateInstance(typ:Int,owners:Array[OwnerReference]) =	{	
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		val newInst=StorageManager.createInstance(typ,owners)
		ActionList.addTransactionData(newInst.ref,new CreateAction(newInst.ref ))
		
		// notify owners
		for(owner <-owners)
		{
			internAddPropertyToOwner(newInst.ref,owner)
		}
		newInst
	}
	
		
	// internal routine
	private def internAddPropertyToOwner(ref:Reference,owner:OwnerReference) = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {
				// Property data found				
				case Some(a) => a 
				// so far no Property data there, create an empty one
				case _ => StorageManager.createInstanceProperties(owner.ownerRef)
			} // and add the new child to the prop list
			).addChildInstance(owner.ownerField ,ref) 
			
		  ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp)))
	}
	
	private def tryWriteInstanceData(data:InstanceData) =	{
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		ActionList.addTransactionData(data.ref,new DataChangeAction (Some(data) ))
	}
	
	/**
	 *  @param ref Reference of the instance to be changed
	 *  @param fieldNr number of the data field to be changed
	 *  @param newValue the new Value
	 */
	def tryWriteInstanceField(ref:Reference,fieldNr:Byte,newExpression:Expression):Boolean = {
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		val instD=ActionList.getInstanceData(ref)
		
		// check for FieldReferences
		val oldRefList=instD.fieldData(fieldNr).getFieldReferences(Nil)
		val newRefList=newExpression.getFieldReferences(Nil)	
		// set the cache value of the FieldReference expressions in the new Expression
		for(nr<-newRefList)
		{
			val sourceInst = resolveLinkRef(ref,nr)
		  val sourceValue=ActionList.getInstanceData(sourceInst).fieldValue(nr.remField )
		  nr.cachedValue =sourceValue	
		}		
		
				// remove all ReferencingLinks data off the removed refs
		val removedRefs=findMissingRefs(oldRefList,newRefList)
		println("Removed Refs:" +removedRefs)
		for( r <- removedRefs)
			removeLinkRef(ref,r)
	  // add new ReferencinLinks data for added refs
		val addedRefs=findMissingRefs(newRefList,oldRefList)
		println("added Refs: "+addedRefs)
		for( r <- addedRefs)
		  addLinkRef(ref,fieldNr,r)
			 
		val oldValue=instD.fieldData(fieldNr).getValue // safe the old field value 				  
		// store data		
		ActionList.addTransactionData(ref,new DataChangeAction (Some(instD.setField(fieldNr,newExpression)) ))
		
		// pass on the changed value to referencing instances
		val newValue=newExpression.getValue
		if(newValue!=oldValue) passOnChangedValue(ref,fieldNr,newValue)
		true
	}
	
	
		
	/** notifies all target instances of the source instante that the data field has changed
	 *  @param sourceRef instance that has changed
	 *  @param fieldNr field nr that has changed
	 *  @param newValue the new value of the field 
	 */
	private def passOnChangedValue(sourceRef:Reference,fieldNr:Byte,newValue:Constant):Unit ={
		ActionList.getReferencingLinks(sourceRef) match {			
			case Some(refLinks) => {				
				for (r <- refLinks.links(fieldNr)) // get all reflinks for the changed field
				   notifyDataChangeToReferencingInst(r,sourceRef,fieldNr,newValue) // notify them
			}
			case _ => // go drink a beer
		}
	}	
	
	
	
	/** notifies ONE target instance that the source instance field was modified and stores it 
	 *  @param targetFieldRef instance and field of the target instace that should be notified
	 *  @param sourceRef the instance that has changed
	 *  @param sourceField the field that has changed
	 *  @param newValue the new Value of that field
	 */
	private def notifyDataChangeToReferencingInst(targetFieldRef:ExtFieldRef,sourceRef:Reference,sourceField:Byte,newValue:Constant) = 	{
		val targetRef=targetFieldRef.getReference
		val targetData=ActionList.getInstanceData(targetRef)
		val theField=targetData.fieldData(targetFieldRef.field )
		val oldRemoteValue=theField.getValue // result value before changing the cache value
		val refList=theField.getFieldReferences(Nil) // all field references in the term
		
		// find the reference(s) that point the the source instance
		for(fRef <-refList; // check all references in the term
		  if(resolveLinkRef(targetRef,fRef)==sourceRef && // if the reference points to the source instance
		  	 fRef.remField==sourceField	)) { // and to the source field 
			  fRef.setCachedValue(newValue)			
		}
		targetData.regenFieldCache(targetFieldRef.field)
		// now calculate the new value of this field
		val newRemoteValue=theField.getValue
		if(newRemoteValue!=oldRemoteValue){ // if the value has changed after the change of the referenced field
			//pass on the changed value of the target instance to it's targets
			passOnChangedValue(targetRef,targetFieldRef.field,newRemoteValue)			
		}
		// save the target instance with the new cached value (could be optimized when cachevalue does not change)
		ActionList.addTransactionData(targetRef,new DataChangeAction(Some(targetData)))
	}
	
	 
	
	
	
	// Checks the  reference lists of the new value and the old value, what references were removed in the old one	
	private def findMissingRefs(oldList:List[FieldReference],newList:List[FieldReference]) =
	{		
		var resultList:List[FieldReference]=Nil
		for(r <-oldList)
			if(!newList.contains(r)) resultList= r :: resultList
		resultList
	}	
	
	/**  resolves the real source ref, when the type or instance field is not set in a FieldReference
	 * @param targetRef the "this" instance what contains the field with the FieldReference
	 * @param sourceRef the FielReference pointing to the source. When this reference dont have type or instance
	 * information, take the targetRef information
	 */
	private def resolveLinkRef(targetRef:Reference,sourceRef:FieldReference):Reference =
	{
	  if(sourceRef.remType==None ) 		{
			if(sourceRef.remInst==None) return targetRef // reference to a fild in THIS instance, so no more action here
			else new Reference(targetRef.typ,sourceRef.remInst.get)
		}
		else new Reference(sourceRef.remType.get,sourceRef.remInst.get)
	}
	
	/** removes the external target link information from the source instance
	 *  @param targetRef reference to the target object pointing to the source
	 *  @param sourceRef the source instance where the target info is to be deleted 
	 */
	private def removeLinkRef(targetRef:Reference,sourceRef:FieldReference):Unit ={				
		val sourceInst=resolveLinkRef(targetRef,sourceRef)
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) => linkData
			case _ => throw new IllegalArgumentException ("Cant find Referencing links for "+sourceInst + 
				" when trying to remove FieldReference in "+targetRef)
		} ).removeTargetLink(targetRef,sourceRef.remField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))		
	}
	
	
	/** adds new external link information to the source instance
	 * @param targetRef the new target instance pointing the the source instnace
	 * @param targetField in what field in the target instance is the new FieldReference
	 * @param sourceRef the FieldReference pointing to the source instance 
	 */
	private def addLinkRef(targetRef:Reference,targetField:Byte,sourceRef:FieldReference):Unit = {
		val sourceInst = resolveLinkRef(targetRef,sourceRef) // resolve missing type and instance information
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) => linkData
			case _ => new ReferencingLinks(sourceInst,Map())				
		} ).addTargetLink(ExtFieldRef(targetRef.typ,targetRef.instance ,targetField),sourceRef.remField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))
		
		//TODO check against circular references !!!
	}
	
	
	
	
	/**
	 * @param ref the instance to be deleted
	 * @param dontNotifyOwner When notifying the parents of that instance, ignore the given parent
	 */
	def tryDeleteInstance(ref:Reference,dontNotifyOwner:Option[Reference]):Unit =	{
  	if(!running ) throw new IllegalArgumentException("No transaction defined ")
  	val instD=ActionList.getInstanceData(ref)
  	ActionList.addTransactionData(ref,new DeleteAction(ref ))

  	// notify owners
  	for(owner <-instD.owners)
  	{
  		if(dontNotifyOwner match {case Some(dno)=> owner!=dno;case _ =>true}) {			
  			internRemovePropertyFromOwner(ref,owner)
  		}
  	}
  	
  	// remove link information at external source instances
  	for(afield <-instD.fieldData) // check all fields for FieldReferences
  	{
  		val refList= afield.getFieldReferences(Nil) // get all FielReferences from that term
  		for(fref <-refList) // remove all external links
  			removeLinkRef(ref,fref)
  	}
  	// remove link information from external target instances pointing here
  	ActionList.getReferencingLinks(ref) match {			
			case Some(refLinks) => {				
				for ((fieldNr,list) <- refLinks.links) //  walk through all fields
					  for(aref <- list) // wall through the list for a field
					  {
					  	val targetRef=aref.getReference
					  	if(targetRef!=ref) // ignore if the link points from the same instance
					  	{
					  		val targetData=ActionList.getInstanceData(targetRef)
					  		val theField=targetData.fieldData(aref.field ). // get the term that contains the linkref
					  		replaceFieldRefWithValue({ // go through all elements and searches fieldRef elements
					  			aFieldRef =>{ val relink=resolveLinkRef(targetRef,aFieldRef);
					  				 						println("checking ref:"+aFieldRef+" resolved:"+relink+" with:"+ref);
					  				 						relink==ref
					  				 } // compares a fieldref with this instance
					  			// if it is the same, return true. That will make the LinkRef replace itself with the cached Value					  		
					  		})					  		
					  		ActionList.addTransactionData(targetRef,new DataChangeAction(Some(targetData.setField(aref.field, theField)))) // store that target
					  	}
					  }
				   
			}
			case _ => // go drink a beer
		}
  	//TODO delete instance from caches !!!
  	
  	// delete Children
  	ActionList.getInstanceProperties(ref) match {
  		case Some(propdat) => 
  		{
  			val myRef=Some(ref)
  		  for (pfield <-propdat.propertyFields; child <-pfield.propertyList )
  		  	tryDeleteInstance(child,myRef)
  		}
  		case None => 
  	}
  }
	
	
	/** moves one instance to another owner
	 *  @param subRef Instance to be moved
	 *  @param fromOwner old owner who will loose the subinstance
	 *  @param toOwner new owner who will get the subinstance
	 */
	def tryMoveInstance(subRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference):Unit = {
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		// change the property information
		internRemovePropertyFromOwner(subRef,fromOwner)
		internAddPropertyToOwner(subRef,toOwner)
		
		// change the owner ref inside of the instance data
		val instData=ActionList.getInstanceData(subRef)
		ActionList.addTransactionData(subRef, new DataChangeAction(Some( instData.changeOwner(fromOwner,toOwner)),None,None))
	}
	
	
	def tryCopyInstance(instRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference):Unit = {
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		val instD=ActionList.getInstanceData(instRef)
		// get the other owners of that instance, apart from "fromOwner", and add the new owner toOwner
		val newOwners:Array[OwnerReference]= instD.owners.filter(x => x!=fromOwner) :+ toOwner
		var createInst=tryCreateInstance(instRef.typ ,newOwners) // new instance created by DB
		
		createInst=instD.clone(createInst.ref,newOwners)
		
		// Register FieldReferences
		for(i <- 0 until instD.fieldData.size)
		{
			val refList=instD.fieldData(i).getFieldReferences(Nil)
			for(rf <-refList)
				 addLinkRef(createInst.ref,i.toByte,rf)
		}
		ActionList.addTransactionData(createInst.ref,new DataChangeAction(Some(createInst)))
		
		// now copy all owned child instances
		ActionList.getInstanceProperties(instRef) match {
			case Some(prop) => { // if this instances has child instances
				for(fieldNr <- 0 until prop.propertyFields.length) // go through all prop fields
				{
					val fieldData=prop.propertyFields(fieldNr) // get the data for the prop field
					for(childRef <-fieldData.propertyList) // go through all children of that field
						tryCopyInstance(childRef,new OwnerReference(fieldNr.toByte,instRef), // copy them to the new instance
							new OwnerReference(fieldNr.toByte,createInst.ref))
				}
			}
			case _ => {} // if no children, do nothing
		}
	}
	
	
	// internal routine
	private def internRemovePropertyFromOwner(subRef:Reference,fromOwner:OwnerReference)
	{
		val newProp= (ActionList.getInstanceProperties(fromOwner.ownerRef) match {
  				// Property data found				
  				case Some(a) => a 
  				// so far no Property data there, something is wrong here
  				case _ => throw new IllegalArgumentException("Delete "+subRef+" /Notify Owner but owner "+fromOwner.ownerRef+" has no Property data ")
  			} // and add the new child to the prop list
  			).removeChildInstance(fromOwner.ownerField ,subRef)
  	ActionList.addTransactionData(fromOwner.ownerRef,DataChangeAction(None,Some(newProp)))
	}
	
	/**
	 *  encapsulates a transaction so that StartTransaction and Finishtransaction/BreakTransaction
	 *  will always be executed
	 */
	def doTransaction (f :  => Unit):Boolean = {
    startTransaction()
    var success=true
    try {
        f
    } catch { case e:Exception => {e.printStackTrace(); success=false; breakTransaction()}   }
    if(success) finishTransaction()
    success
}
	

}


