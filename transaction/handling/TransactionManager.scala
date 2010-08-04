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
		println("Start Trans " +TransLogHandler.transID)
	  if(running ) throw new IllegalArgumentException("An Transaction is still running ")
		running=true
	  TransLogHandler.incrementTransID();
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
		ActionList.addTransactionData(data.ref,new DataChangeAction (Some(data),None ))
	}
	
	/**
	 *  @param ref Reference of the instance to be changed
	 *  @param fieldNr number of the data field to be changed
	 *  @param newValue the new Value
	 */
	def tryWriteInstanceField(ref:Reference,fieldNr:Byte,newValue:Expression):Boolean = {
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		val instD=ActionList.getInstanceData(ref)
		
		// check for FieldReferences
		val oldRefList=instD.fieldData(fieldNr).getFieldReferences(Nil)
		val newRefList=newValue.getFieldReferences(Nil)		
				// remove all ReferencingLinks data of the removed refs
		val removedRefs=findMissingRefs(oldRefList,newRefList)
		for( r <- removedRefs)
			removeLinkRef(ref,r)
	  // add new ReferencinLinks data for added refs
		val addedRefs=findMissingRefs(newRefList,oldRefList)	
		for( r <- addedRefs)
		  addLinkRef(ref,r)
		  
		// store data
		instD.setField(fieldNr,newValue)
		ActionList.addTransactionData(ref,new DataChangeAction (Some(instD) ))
		
		return false
	}
	
	// Checks the  reference lists of the new value and the old value, what references were removed in the old one
	private def findMissingRefs(oldList:List[FieldReference],newList:List[FieldReference]) =
	{
		var resultList:List[FieldReference]=Nil
		for(r <-oldList)
			if(!newList.contains(r)) resultList= r :: resultList
		resultList
	}	
	
	// resolves the real source ref, when the type or instance field is not set in a FieldReference
	private def resolveLinkRef(targetRef:Reference,sourceRef:FieldReference):Reference =
	{
	  if(sourceRef.remType==None ) 		{
			if(sourceRef.remInst==None) return targetRef // reference to a fild in THIS instance, so no more action here
			else new Reference(targetRef.typ,sourceRef.remInst.get)
		}
		else new Reference(sourceRef.remType.get,sourceRef.remInst.get)
	}
	
	// removes the external link information from the source instance
	private def removeLinkRef(targetRef:Reference,sourceRef:FieldReference):Unit ={				
		val sourceInst=resolveLinkRef(targetRef,sourceRef)
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) => linkData
			case _ => throw new IllegalArgumentException ("Cant find Referencing links for "+sourceInst + 
				" when trying to remove FieldReference in "+targetRef)
		} ).removeTargetLink(targetRef,sourceRef.remField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))		
	}
	
	// adds new external link information to the source instance
	private def addLinkRef(targetRef:Reference,sourceRef:FieldReference):Unit = {
		val sourceInst = resolveLinkRef(targetRef,sourceRef)		
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) => linkData
			case _ => new ReferencingLinks(sourceInst,Map())				
		} ).addTargetLink(targetRef,sourceRef.remField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))
		// set the cache value of the FieldReference expressions
		val sourceValue=ActionList.getInstanceData(sourceInst).fieldValue(sourceRef.remField )
		sourceRef.cachedValue =sourceValue
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
		//val instD=ActionList.getInstanceData(subRef)
		internRemovePropertyFromOwner(subRef,fromOwner)
		internAddPropertyToOwner(subRef,toOwner)
	}
	
	
	def tryCopyInstance(instRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference):Unit = {
		if(!running ) throw new IllegalArgumentException("No transaction defined ")
		val instD=ActionList.getInstanceData(instRef)
		// get the other owners of that instance, apart from "fromOwner", and add the new owner toOwner
		val newOwners:Array[OwnerReference]= instD.owners.filter(x => x!=fromOwner) :+ toOwner
		val createInst=tryCreateInstance(instRef.typ ,newOwners) // new instance created by DB
		val newInst=instD.clone(createInst.ref ,newOwners) // make a copy of the existing instance
		tryWriteInstanceData(newInst)
		
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


