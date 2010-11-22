/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import server.storage._
import server.config._
import definition.typ._
import definition.data._
import definition.expression._
import server.comm._
import server.test.SimpleProfiler


/** manages Transaction handling
 * 
 */
object TransactionManager {
  @volatile var running=false
  
  val numUndoSteps=9
  var currentUser:Short=0
  var currentActionCode:Short=0
  var currentRef:Reference=null
  var multiInst:Boolean=false
  var logCreateType:Int=0
  
	
	private val transLock : AnyRef = new Object()	
  
 
			
	@volatile var undoUserEntry:UserEntry=null
	
	// Starts a new Transaction
	private def startTransaction() =	{		
	  if(running ) throw new IllegalArgumentException("An Transaction is still running ")
		running=true
	  TransLogHandler.incrementTransID();
	  //println("Start Trans " +TransLogHandler.transID)
	}
	
	// Finishes the Transaction and commits all changes to the database
	private def finishTransaction() = 	{
		
		if(!running) throw new IllegalArgumentException("Finish: No transaction running ")		
		ActionList.commitAllData()
		TransDetailLogHandler.log(TransLogHandler.transID,currentUser,currentRef,multiInst,currentActionCode,logCreateType)
		//println("Finish Trans "+ TransLogHandler.transID)
		running=false
	}
	
	def canModify = running && undoUserEntry==null
	
	
	// breaks an transaction if an error occurs during the transaction
	 def breakTransaction() =	{
		 ActionList.breakAllData()		
		TransLogHandler.resetTransID()
		running=false
	 }
	 
	 def requestUndoData(user:UserEntry) = {
		 if(undoUserEntry!=null||running){ // already undoing
			 user.thread.denyUndoRequest
		 } else {
			 undoUserEntry=user
			 ActiveUsers.lockUsersForUndo(user)
			 val currTransID=TransLogHandler.transID			 
			 user.thread.sendUndoInformation( TransDetailLogHandler.readTransStepData(currTransID,currTransID-numUndoSteps))			 
		 }
	 }
	 
	 def stopUndo(user:UserEntry) = {
		 if(undoUserEntry==null) println(" Undo process not running !")
		 else if (user.info .id!=undoUserEntry.info.id) println("UNDO user "+undoUserEntry.info+" but stopped by "+user.info)
		 else {
			 ActiveUsers.releaseUsersForUndo(user)
			 undoUserEntry=null
		 }
	 }
		
	
	
	/** creates a new Instance
	 * @param typ the class id of the new instance
	 * @param notifyRefandColl should referencing links and collFuncs be notified about the new instance
	 * @param notifySubs notify subscribers of the parent inst that the new instance was added
	 */
	def tryCreateInstance(typ:Int,owners:Array[OwnerReference],notifyRefandColl:Boolean,pos:Int= -1,notifySubs:Boolean=true,withStartValues:Boolean=true) =	{	
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		//TODO: Check if the child type is allowed in the owner property fields
		
		var newInst=StorageManager.createInstance(typ,owners,withStartValues)	
		var collData:Option[CollFuncResultSet]=None
		var newLinkData:ReferencingLinks=null
		val copyInfo:Option[CopyMoveInfo]=if(!notifySubs) AddDontNotifyOwners.some  else None
		for(i<-newInst.fieldData.indices;val expr=newInst.fieldData(i);if(!expr.isEmpty)){
			// check new collcalls
			val newCollCalls=expr.getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
  		  if(!newCollCalls.isEmpty) {
			   val (acollData,anExpression)=TransactionManager.addCollCalls(null, newInst.ref, newCollCalls, i.toByte,expr)
			   newInst=newInst.setField(i.toByte,anExpression)
			   collData=Some(acollData)	
  		 }
			// check new links
			val newRefList:Seq[FieldReference]=expr.getElementList[FieldReference](DataType.FieldRefTyp,Nil)
			for(nr<-newRefList)	{
				val sourceInstRef = resolveLinkRef(newInst.ref,nr)
				val sourceValue=(
						if(sourceInstRef==newInst.ref) newInst 
						else  ActionList.getInstanceData(sourceInstRef)	).fieldValue(nr.remField )
						nr.cachedValue =sourceValue	//side effect !!! should be changed !
				//addLinkRef(newInst.ref,i.toByte,nr)
				if(newLinkData==null)newLinkData=		new ReferencingLinks(sourceInstRef,Map())
				newLinkData=newLinkData.addTargetLink(ExtFieldRef(newInst.ref.typ,newInst.ref.instance ,i.toByte),nr.remField)
			}
		}		  
		  
		ActionList.addTransactionData(newInst.ref,new CreateAction(newInst.ref,Some(newInst),None,
			if(newLinkData==null)None else Some(newLinkData),collData,copyInfo))		
		// notify owners
		for(owner <-owners)
		{
			internAddPropertyToOwner(newInst,owner,pos,!notifySubs)
		}		
		if(notifyRefandColl)
			for(i <- 0 until newInst.fieldData.size;if (!newInst.fieldData(i).isEmpty))
				passOnChangedValue(newInst,i.toByte,EMPTY_EX,newInst.fieldData(i).getValue)
		//SimpleProfiler.measure("notif owner")
		newInst
	}
	
		
	// internal routine
	private def internAddPropertyToOwner(newInst:InstanceData,owner:OwnerReference,pos:Int,refreshDestination:Boolean=false) = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {
				// Property data found				
				case Some(a) => a 
				// so far no Property data there, create an empty one
				case _ => StorageManager.createInstanceProperties(owner.ownerRef)
			} // and add the new child to the prop list
			).addChildInstance(owner.ownerField ,newInst.ref,pos) 
			
		  ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp),None,None, if(refreshDestination)RefreshDestinationOwner.some else None))		  
	}
	
	private def movePropertyToPos(ref:Reference,owner:OwnerReference,pos:Int) = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {
  				// Property data found				
  				case Some(a) => a 
  				// so far no Property data there, something is wrong here
  				case _ => throw new IllegalArgumentException("move pos "+ref+"  but owner "+owner.ownerRef+" has no Property data ")
  			} // and add the new child to the prop list
  			).moveChildInstanceToPos(owner.ownerField ,ref,pos)
  	ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp),None,None,
  		 Some(RefreshDestinationOwner) ))
	}
	
	def tryWriteInstanceData(data:InstanceData) =	{
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		ActionList.addTransactionData(data.ref,new DataChangeAction (Some(data) ))
	}
	
	/**
	 *  @param ref Reference of the instance to be changed
	 *  @param fieldNr number of the data field to be changed
	 *  @param newValue the new Value
	 */
	def tryWriteInstanceField(ref:Reference,fieldNr:Byte,newExpression:Expression):Boolean = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		//SimpleProfiler.startMeasure("Change inst")
		var theExpression=newExpression
		val instD=ActionList.getInstanceData(ref)
		//SimpleProfiler.measure("load")
		// check for FieldReferences
		val oldRefList=instD.fieldData(fieldNr).getElementList[FieldReference](DataType.FieldRefTyp,Nil)
		val newRefList:Seq[FieldReference]=newExpression.getElementList[FieldReference](DataType.FieldRefTyp,Nil)	
		// set the cache value of the FieldReference expressions in the new Expression
		for(nr<-newRefList)
		{
			val sourceInstRef = resolveLinkRef(ref,nr)
		  val sourceValue=ActionList.getInstanceData(sourceInstRef).fieldValue(nr.remField )
		  nr.cachedValue =sourceValue	//side effect !!! should be changed !
		}				
		// remove all ReferencingLinks data off the removed refs
		val removedRefs=findMissingElements[FieldReference](oldRefList,newRefList)
		
		for( r <- removedRefs)
			removeLinkRef(ref,r)
	  // add new ReferencinLinks data for added refs
		val addedRefs=findMissingElements[FieldReference](newRefList,oldRefList)
		
		for( r <- addedRefs)
		  addLinkRef(ref,fieldNr,r)
		//SimpleProfiler.measure("refCheck")
		 // Check for CollFunctionCalls 
		val oldCollCalls= instD.fieldData(fieldNr).getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
		if( !oldCollCalls.isEmpty) println("oldCollCalls "+oldCollCalls)
		val newCollCalls=newExpression.getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
		if( !newCollCalls.isEmpty) println("newCollCalls "+newCollCalls)
		 
		// Remove all CollResults of the removed CollCalls
		val removedCalls=findMissingElements[CollectingFuncCall](oldCollCalls,newCollCalls)
		val remCollData:CollFuncResultSet= if(! removedCalls.isEmpty) removeCollCalls(ref,removedCalls,fieldNr)
		      else null
		// add new CollResults
		val newCalls=findMissingElements[CollectingFuncCall](newCollCalls,oldCollCalls)
		val newCollData = if(!newCalls.isEmpty) {
			  val (acollData,anExpression)=addCollCalls(remCollData, ref, newCalls, fieldNr,newExpression)
			  theExpression=anExpression // update the Expression			  
			  acollData
		} else remCollData
			    
		if (newCollData!=null) ActionList.addTransactionData(ref,new DataChangeAction(None,None,None,Some(newCollData))) 
		  
		//safe the old field value  
		val oldValue=instD.fieldData(fieldNr).getValue   				  
		// set field in instance to new expression
		val newInst=instD.setField(fieldNr,theExpression)
		// store data
		ActionList.addTransactionData(ref,new DataChangeAction (Some(newInst) ))
		
		// pass on the changed value to referencing instances
		val newValue=theExpression.getValue
		if(newValue!=oldValue) passOnChangedValue(newInst,fieldNr,oldValue,newValue)
		//SimpleProfiler.measure("passOn")
		true
	}
	
	
		
	/** notifies all target instances of the source instante that the data field has changed
	 *  @param sourceRef instance that has changed
	 *  @param fieldNr field nr that has changed
	 *  @param newValue the new value of the field 
	 */
	private def passOnChangedValue(newInst:InstanceData,fieldNr:Byte,oldValue:Constant,newValue:Constant):Unit ={
		
		// Check for other instances referencing to this instance
		ActionList.getReferencingLinks(newInst.ref) match {			
			case Some(refLinks) => {		
				if (refLinks.links.contains(fieldNr))
				for (r <- refLinks.links(fieldNr)) // get all reflinks for the changed field
				   notifyDataChangeToReferencingInst(r,newInst.ref,fieldNr,newValue) // notify them
			}
			case _ => // go drink a beer
		}
		// Check for parent instances having collectingFunctions on this instance
		for(owner <-newInst.owners ) {
			ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) => {
					notifyCollFunc_ChildChanged(owner ,collData,newInst.ref,fieldNr,oldValue,newValue)
				}
				case _ => // another beer
			}
		}
		// notify Subscriptions
		//CommonSubscriptionHandler.instanceChanged(newInst)
	}	
	
	private def passOnNewInstanceToCollFuncParents(newInst:InstanceData,ownerList:Seq[OwnerReference])
	{
		for(owner <-ownerList ) {
			ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) => {
					for(i <- 0 until newInst.fieldData.size;if (newInst.fieldData (i)!=EMPTY_EX))
					notifyCollFunc_ChildChanged(owner ,collData,newInst.ref,i.toByte,EMPTY_EX,newInst.fieldData(i).getValue)
				}
				case _ => // another beer
			}
		}
	}
	
	
	private def passOnDeletedInstanceToCollFuncParents(instD:InstanceData,ownerList:Seq[OwnerReference]) = {
		for(owner <-ownerList) 	{
  		//println(" "+owner.ownerRef)
  		ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) => {
					//println("notify CollFunc Child Deleted, owner:"+owner+" child"+instD.ref)
					notifyCollFunc_ChildDeleted(owner ,collData,instD)
				}
				case b => //println("check "+owner+" for Colldata:"+b) // more beer !
			}
  	}
	}
	
	// **************************** FieldReference Management **************************************************************
	
	
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
		val refList=theField.getElementList[FieldReference](DataType.FieldRefTyp,Nil) // all field references in the term
		
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
			passOnChangedValue(targetData,targetFieldRef.field,oldRemoteValue,newRemoteValue)			
		}
		// save the target instance with the new cached value (could be optimized when cachevalue does not change)
		ActionList.addTransactionData(targetRef,new DataChangeAction(Some(targetData)))
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
	 * @param targetRef the new target instance pointing to the source instance
	 * @param targetField in what field in the target instance is the new FieldReference
	 * @param sourceRef the FieldReference pointing to the source instance 
	 */
	private def addLinkRef(targetRef:Reference,targetField:Byte,sourceRef:FieldReference):Unit = {		
		val sourceInst = resolveLinkRef(targetRef,sourceRef) // resolve missing type and instance information
		println("addLinkRef targetRef:"+targetRef+" targetField:"+targetField+" sourceRef:"+
			sourceRef+ " sourceInst:"+sourceInst)
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) =>{println("Linkdata already there:"+linkData); linkData}
			case _ => new ReferencingLinks(sourceInst,Map())				
		} ).addTargetLink(ExtFieldRef(targetRef.typ,targetRef.instance ,targetField),sourceRef.remField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))
		
		//TODO check against circular references !!!
	}
	
	//**************************************  CollectingFunction management **************************************
	
	/** removes  function call results from the result set
	 * @param ref Reference of the parent instace
	 * @param oldCollCalls the list of calls to be removed
	 * @param pFieldNr the field where the calls were stored
	 * 
	 */
	private def removeCollCalls(ref:Reference,oldCollCalls:List[CollectingFuncCall],pFieldNr:Byte):CollFuncResultSet ={				
		//val sourceInst=resolveLinkRef(targetRef,sourceRef)
		var collData= ( ActionList.getCollData(ref) match {
			case Some(collData) => collData
			case _ => throw new IllegalArgumentException ("Cant find CollFunkData for "+ref + 
				" when trying to remove CollFunks "+oldCollCalls)
		} )
		for(r <- oldCollCalls)
		   collData=collData.removeCollCall(r,pFieldNr)
		collData		
	}
	
	/** adds new collFuncCalls to the collFuncResultSet
	 *  @param collData current version of the ResultSet
	 *  @param ref Reference of the parent instance
	 *  @param newCollCalls the calls inside of a parentfield that are to be added
	 *  @param pfieldNr in what field were the calls added
	 *  @return tuple: (the new result set, the updated expression)
	 */
	def addCollCalls(collData:CollFuncResultSet,ref:Reference,newCollCalls:List[CollectingFuncCall],
	                         pFieldNr:Byte, newTerm:Expression):(CollFuncResultSet,Expression) ={				
		// get the collData
		var theTerm=newTerm
		var theCollData= 
			if(collData==null) 
			  ActionList.getCollData(ref) match {
				 case Some(collData) => collData
				 case _ => new CollFuncResultSet(ref,Nil) // no Colldata there, create new			         
			  } 
		  else collData
		// for each new call  
		for(nc <- newCollCalls) { // add it to the resultset
		   val (acollData,resultValue)=theCollData.addCollCall(nc,pFieldNr)
		   theCollData=acollData 
		   theTerm=setFuncResults(theTerm,nc,resultValue) // and put the result into the call expression
		}  
		(theCollData,theTerm)		
	}
	
	/** puts the results of a collfunc into the call expression 
	 * @param newTerm the term containing the funcCall that should be replaced with a call that has the result set
	 * @param call the new function call we are looking for
	 * @param newValue the new value that should be given to the call
	 * @return the updated expression
	 */
	private def setFuncResults(newTerm:Expression,call:CollectingFuncCall,newValue:Constant ):Expression= {
		newTerm.replaceExpression((ex:Expression) => {
		  	 ex match {
		  		 case fc:CollectingFuncCall => {
		  			   println("setFuncResults fc:"+fc + "call:"+call+" =="+(fc==call)+" newValue:"+newValue)
		  			   if(fc==call) return fc.setValue(newValue)
		  			   else return fc
		  			 }
		  		 case a => a 
		  	 }
		   })
	}
	
	/** notifies the owner that the value of a CollFuncCall has changed
	 * @param ownerInst the InstanceData of the owner
	 * @param collData the collFuncData of the functionCall that should be changed
	 * @param newValue the new result of the functionCall that should be set
	 * @return a new version of the owner's InstanceData
	 */
	private def updateOwnerCollFunc(ownerInst:InstanceData,collData:CollFuncResult,newValue:Constant):InstanceData = {
		//println("updateOwnerCollFunc "+ownerInst.fieldData(collData.parentField ))
		val newExpression=ownerInst.fieldData(collData.parentField ).replaceExpression((ex:Expression) => {
		  	 ex match {
		  		 case fc:CollectingFuncCall => {
		  			 //println("UpdateOwnerCollFunc fc:"+fc+ " colldata:"+collData+" newValue:"+newValue)
		  			 if(collData.fitsToFuncCall(fc,collData.parentField)) fc.setValue(newValue)
		  			 else fc		  		 
		  		 }
		  		 case a => a 
		  	 }
		   })
		 //println(" newExpr:"+newExpression)  
		 ownerInst.setField(collData.parentField,newExpression)
	}
	
	
	/*private def notifyCollFunc_ChildAdded(owner:OwnerReference,collData:CollFuncResultSet,childInst:InstanceData)= {
		val myClass=AllClasses.get.getClassByID(childInst.ref.typ)
		var matches=false
		// check if the child matches to any of the collFuncResults 
		for(res <-collData.callResultList )
		   if( res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType)) matches=true
		
		if(matches) {  // if yes, update the changes
			var parentInstData=ActionList.getInstanceData(owner.ownerRef)
			val newCollDataList=
			(for(res <-collData.callResultList )
				yield if( res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType))  {
		  	      val (newRes,value) = collData.childChanged(res,childInst.ref,EMPTY_EX,
		  	      	childInst.fieldData(res.childField).getValue)
		  	      parentInstData=updateOwnerCollFunc(parentInstData,newRes,value)		  	      
		  	      newRes
		    }	
		  	else res).toList		
		  val newResultSet=new CollFuncResultSet(owner.ownerRef,newCollDataList)
		  ActionList.addTransactionData(owner.ownerRef ,new DataChangeAction(Some(parentInstData),None,None,Some(newResultSet)))		 
		}		
	}*/
	
	
	/** notifies one parent instance that a child was changed, eventually calculates the new values and stores the changes
	 * @param owner Reference to the owner instance
	 * @param collData the CollFuncResultSet of the owner instance
	 * @param childRef the Reference of the changed child
	 * @param childField the number of the field that was changed
	 * @param oldValue the old value of the field
	 * @param newValue the new value of the field
	 * 
	 */
	private def notifyCollFunc_ChildChanged(owner:OwnerReference,collData:CollFuncResultSet,childRef:Reference,childField:Byte,
	                                       oldValue:Constant,newValue:Constant)= {
		val myClass=AllClasses.get.getClassByID(childRef.typ)
		
		var fieldMatchSet:Set[Int]=Set.empty
		// check if the child matches to any of the collFuncResults 
		
		for(res <-collData.callResultList ){
			//println("myclass "+ myClass+ " res.childType:"+res.childType+ " " +myClass.inheritsFrom(res.childType))
			if(res.childField ==childField && res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType)) fieldMatchSet=fieldMatchSet+ res.parentField
		}
		   
		
		if(!fieldMatchSet.isEmpty) {  // if yes, update the changes			
			//println("fieldMatchSet: "+fieldMatchSet)
			var parentInstData=ActionList.getInstanceData(owner.ownerRef)
			val oldParentValues:Array[Constant]= new Array[Constant](parentInstData.fieldData .size)
				for(i <-fieldMatchSet) oldParentValues(i)= parentInstData.fieldValue(i)
			val newCollDataList=
			(for(res <-collData.callResultList )
				yield if(res.childField ==childField && res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType))  {
		  	      val (newRes,value) = collData.childChanged(res,childRef,oldValue,newValue)		  	      
		  	      parentInstData=updateOwnerCollFunc(parentInstData,newRes,value)		  	      
		  	      newRes
		    }	
		  	else res).toList		
		  val newResultSet=new CollFuncResultSet(owner.ownerRef,newCollDataList)
		  ActionList.addTransactionData(owner.ownerRef ,new DataChangeAction(Some(parentInstData),None,None,Some(newResultSet)))
		  // pass on to owners
		  for(fieldNr <-fieldMatchSet;if(parentInstData.fieldData(fieldNr).getValue!=oldParentValues(fieldNr)))
		  	passOnChangedValue(parentInstData,fieldNr.toByte,oldParentValues(fieldNr),parentInstData.fieldData(fieldNr).getValue)
		}		
	}
	
	
	private def notifyCollFunc_ChildDeleted(owner:OwnerReference,collData:CollFuncResultSet,childInstance:InstanceData) = {
		val myClass=AllClasses.get.getClassByID(childInstance.ref.typ)
		var fieldMatchSet:Set[Int]=Set()
		for(res <-collData.callResultList )
		{
			//println("res :"+res)
			if( res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType)) fieldMatchSet=fieldMatchSet+ res.parentField
		}
		if(!fieldMatchSet.isEmpty) {
			//println("matches")
			var parentInstData=ActionList.getInstanceData(owner.ownerRef)
			if (parentInstData!=null)
			{
				val oldParentValues:Array[Constant]= new Array[Constant](parentInstData.fieldData .size)
				for(i <-fieldMatchSet) oldParentValues(i)= parentInstData.fieldValue(i)
				val newCollDataList=
					(for(res <-collData.callResultList )
						yield if( res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
								myClass.inheritsFrom(res.childType))  {
							val (newRes,value) = collData.childDeleted(res,childInstance.ref ,childInstance.fieldValue(res.childField ))
							//println("ChildDeleted newRes:"+newRes+ " new Value:"+value)
							parentInstData=updateOwnerCollFunc(parentInstData,newRes,value)		  	      
							newRes
						}	
						else res).toList
						val newResultSet=new CollFuncResultSet(owner.ownerRef,newCollDataList)
				ActionList.addTransactionData(owner.ownerRef ,new DataChangeAction(Some(parentInstData),None,None,Some(newResultSet)))
				// pass on to owners
				for(fieldNr <-fieldMatchSet;if(parentInstData.fieldData(fieldNr).getValue!=oldParentValues(fieldNr)))
					passOnChangedValue(parentInstData,fieldNr.toByte,oldParentValues(fieldNr),parentInstData.fieldData(fieldNr).getValue)
			}
		}
	}
	
	
	// Checks the  lists of the new value and the old value, what elements were removed in the new one	
	private def findMissingElements[T <: Expression](oldList:Seq[T],newList:Seq[T]) =
	{		
		var resultList:List[T]=Nil
		for(r <-oldList)
			if(!newList.contains(r)) resultList= r :: resultList
		resultList
	}	
	
	/**
	 * @param ref the instance to be deleted
	 * @param dontNotifyOwner When notifying the parents of that instance, ignore the given parent
	 */
	def tryDeleteInstance(ref:Reference,dontNotifyOwner:Option[Reference]):Boolean =	{
  	if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
  	val instD=ActionList.getInstanceData(ref)
  	// mark this instance as deleted
  	ActionList.addTransactionData(ref,new DeleteAction(instD))

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
  		val refList= afield.getElementList[FieldReference](DataType.FieldRefTyp,Nil) // get all FielReferences from that term
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
					  		replaceExpression( // go through all elements and searches fieldRef elements
					  			(anExpression:Expression) =>{ anExpression match  {
					  				case aFieldRef:FieldReference => {	
					  					val relink=resolveLinkRef(targetRef,aFieldRef);
					  				  //println("checking ref:"+aFieldRef+" resolved:"+relink+" with:"+ref);
					  				  if (relink==ref) aFieldRef.cachedValue // replace the reference with the cached value
					  				  else aFieldRef // wrong reference, leave it
					  				}
					  				case other => other // dont change any other elements
					  			}
					  				 						
					  				 						
					  				 } // compares a fieldref with this instance
					  			// if it is the same, return true. That will make the LinkRef replace itself with the cached Value					  		
					  		)					  		
					  		ActionList.addTransactionData(targetRef,new DataChangeAction(Some(targetData.setField(aref.field, theField)))) // store that target
					  	}
					  }
				   
			}
			case _ => // go drink a beer			
		}
  	
  	// notify CollFuncs of the parent instances
  	//println("check Coll "+ instD.owners.mkString(", "))
  	passOnDeletedInstanceToCollFuncParents(instD,instD.owners)
  	
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
  	true
  }
	
	def tryMoveMultiInstances(subRefs:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Unit = {
		//println(" move Instances: "+subRefs.mkString(",")+ " from:"+fromOwner+ " to:"+toOwner+" pos:"+atPos)
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		var pos=atPos
	  for(ref <-subRefs) {
	  	tryMoveInstance(ref,fromOwner,toOwner,pos)
	  	if(atPos> -1) pos +=1
	  }
	}
	
	/** moves one instance to another owner
	 *  @param subRef Instance to be moved
	 *  @param fromOwner old owner who will loose the subinstance
	 *  @param toOwner new owner who will get the subinstance
	 */
	def tryMoveInstance(subRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference,pos:Int):Unit = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		// change the property information		
		val instData=ActionList.getInstanceData(subRef)
		val differentOwners= fromOwner!=toOwner
		if(differentOwners) {
			internAddPropertyToOwner(instData,toOwner,pos,true)		
			internRemovePropertyFromOwner(subRef,fromOwner,differentOwners)
			// change the owner ref inside of the instance data
			val newInst=if(differentOwners) instData.changeSingleOwner(fromOwner,toOwner) else instData
			ActionList.addTransactionData(subRef, new DataChangeAction(Some( newInst),None,None,None,Some(ChangeDontNotifyOwners)))
			passOnNewInstanceToCollFuncParents(newInst,List(toOwner))
			passOnDeletedInstanceToCollFuncParents(instData,List(fromOwner))
		} else {
			movePropertyToPos(subRef,fromOwner,pos)
		}
	}
	
	
	def tryCopyMultiInstances(instList:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int) = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		var pos=atPos
		var lastInst:Int=0
		//println("copy Multi :"+instList.mkString(",")+" from:"+fromOwner+" to:"+toOwner+" pos:"+atPos)
		for(ref <-instList) {
			lastInst=tryCopyInstance(ref,fromOwner,toOwner,pos,true)
			if(atPos> -1) pos +=1
		}
		lastInst
	}
	
	def tryCopyInstance(instRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int,
	                    collNotifyOwners:Boolean):Int = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		val instD=ActionList.getInstanceData(instRef)
		// get the other owners of that instance, apart from "fromOwner", and add the new owner toOwner
		if(!instD.owners.contains(fromOwner)) throw new IllegalArgumentException("Copy: instance "+instRef+" is not owned by "+ fromOwner)
		//if(instD.owners.contains(toOwner)) throw new IllegalArgumentException("Copy: instance "+instRef+" is already owned by "+ toOwner)
		val newOwners:Array[OwnerReference]= instD.owners.filter(_ !=fromOwner) :+ toOwner
		var createInst=tryCreateInstance(instRef.typ ,newOwners,false,atPos,false,false) // new instance created by DB
		
		createInst=instD.clone(createInst.ref,newOwners)
		
		// Register FieldReferences to other instances
		for(i <- 0 until instD.fieldData.size;if (instD.fieldData(i)!=EMPTY_EX))
		{
			val refList=instD.fieldData(i).getElementList[FieldReference](DataType.FieldRefTyp,Nil)
			println("try copy inst:"+instRef+" newInst:"+createInst.ref+" "+refList)
			println("old referencing Links "+ActionList.getReferencingLinks(instRef))
			for(rf <-refList)
				 addLinkRef(createInst.ref,i.toByte,rf)
		}
		
		// store data and copy collData to new Instance
		var collData=ActionList.getCollData(instRef)
		for(c <-collData)
			collData=Some(c.changeReference(createInst.ref))
		ActionList.addTransactionData(createInst.ref,new DataChangeAction(Some(createInst),None,None,collData))		
		// notify owners
		if(collNotifyOwners)
		passOnNewInstanceToCollFuncParents(createInst,createInst.owners)
		
		// now copy all owned child instances
		ActionList.getInstanceProperties(instRef) match {
			case Some(prop) => { // if this instances has child instances
				for(fieldNr <- 0 until prop.propertyFields.length) // go through all prop fields
				{
					val fieldData=prop.propertyFields(fieldNr) // get the data for the prop field
					for(childRef <-fieldData.propertyList) // go through all children of that field
						tryCopyInstance(childRef,new OwnerReference(fieldNr.toByte,instRef), // copy them to the new instance
							new OwnerReference(fieldNr.toByte,createInst.ref),-1,false)
				}
			}
			case _ => {} // if no children, do nothing
		}
		createInst.ref.instance 
	}
	
	
	// internal routine
	private def internRemovePropertyFromOwner(subRef:Reference,fromOwner:OwnerReference,notifyOwner:Boolean=false)
	{
		val newProp= (ActionList.getInstanceProperties(fromOwner.ownerRef) match {
  				// Property data found				
  				case Some(a) => a 
  				// so far no Property data there, something is wrong here
  				case _ => throw new IllegalArgumentException("Delete "+subRef+" /Notify Owner but owner "+fromOwner.ownerRef+" has no Property data ")
  			} // and add the new child to the prop list
  			).removeChildInstance(fromOwner.ownerField ,subRef)
  	ActionList.addTransactionData(fromOwner.ownerRef,DataChangeAction(None,Some(newProp),None,None,
  		if(notifyOwner) Some(new RemoveNotifySourceOwners(fromOwner,List(subRef))) else None ))
  	//  notify subscriptions
  	//CommonSubscriptionHandler.instanceDeleted(fromOwner,subRef)
	}
	
	/**
	 *  encapsulates a transaction so that StartTransaction and Finishtransaction/BreakTransaction
	 *  will always be executed
	 */
	def doTransaction (userID:Short,actionCode:Short,ref:Reference,multi:Boolean,createType:Int,f :  => Unit):Option[Exception] = transLock.synchronized{
    startTransaction()
    currentUser=userID
    currentActionCode=actionCode
    currentRef=ref
    multiInst=multi
    logCreateType=createType
    var success=true
    try {
        f
    } catch { case e:Exception => {e.printStackTrace(); success=false; breakTransaction();return Some(e)}   }
    if(success) finishTransaction()   
    None
	}
	
	
	def doUndo(user:UserEntry)= {
		if(undoUserEntry!=null && user.info.id==undoUserEntry.info .id) {
			println("do undo "+TransLogHandler.transID)
			StorageManager.undoLastStep()
			CommonSubscriptionHandler.refreshAfterUndo()
			ActiveUsers.releaseUsersForUndo(user)
			undoUserEntry=null
		}
	}

}


