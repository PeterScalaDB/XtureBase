/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import definition.typ._
import definition.data._
import java.util.NoSuchElementException
import java.io.{DataInput,DataOutput}
import runtime.function._
import definition.expression.FunctionManager
import server.test.SimpleProfiler


/** manages all file io operations
 * 
 */
object StorageManager {
  var ixHandlerList:Map[Int,ClassIndexHandler]=Map()
  val dataFileHandler=new BoolContFileHandler[InstanceData]("InstData.dat",InstanceData.read)
  val propFileHandler=new ContainerFileHandler("PropData.dat",InstanceProperties.read)
  val linkFileHandler=new ContainerFileHandler("ExternLinks.dat",ReferencingLinks.read)
  val collFuncFileHandler=new ContainerFileHandler("collFuncs.dat",CollFuncResultSet.read)
  var shuttedDown=false
  var inited=false
  
  val undoDeleteList=List(TransType.dataChanged.id,TransType.created.id)
  val undoLinkList=List(TransType.linksChanged.id,TransType.created.id)
  val undoPropList=List(TransType.propertyChanged.id,TransType.created.id)
  val undoCollList=List(TransType.collFuncChanged.id,TransType.created.id)
  
  def init(classList:Map[Int,ServerObjectClass] ) =
  {
  	if(ixHandlerList.isEmpty||shuttedDown)
  	{
  		shuttedDown=false
  	  ixHandlerList=ixHandlerList++( for (i <-classList.valuesIterator) yield (i.id -> new ClassIndexHandler(i)))
  	}
  	FunctionManager.setManager(CommonFuncMan)
  }
  
  def getHandler(typ:Int) = try 
  { ixHandlerList(typ) } catch {case e:NoSuchElementException => throw new IllegalArgumentException("Storage: Type "+typ+" not found")}
  
  
  def instanceExists(typ:Int,inst:Int) =getHandler(typ).instanceExists(inst)
  
  
  /** loads an instance from the data file
   * 
   */
  def getInstanceData(ref:Reference):InstanceData =
 	{
  	//print("getInstance "+ref)
  	val handler=getHandler(ref.typ)
  	handler.instCache.getInstanceData(ref.instance ) match
  	{
  		case Some(cacheValue) =>{/*System.out.println(" cacheHit:"+cacheValue);*/ cacheValue}
  		case None =>
  	  {
  	    val rec=handler.getInstanceRecord(ref.instance )
  	    if (rec.dataPos < 1 && rec.dataLength==0) throw new 
  	               IllegalArgumentException("get Instance() instance "+ref+" is deleted")
  	    val instObj=dataFileHandler.readWithBool(ref,rec.dataPos,rec.dataLength,(rec.propPos !=0)&&(rec.propLength !=0) )
  	    //System.out.println(" cacheMiss:"+instObj)
  	    handler.instCache.putInstanceData(instObj)
  	    instObj
  	  }
  	}
  } 
  
  def loadChildren(parent:Reference,ofType:Int,propField:Int) :Seq[InstanceData]= 
  	getInstanceProperties(parent) match 
  	{
  		case Some(props) => {
  			for (child <-props.propertyFields(propField).propertyList;if(child.typ ==ofType))
  				yield getInstanceData(child)
  		}
  		case None=> Seq.empty
  	}
  
  /** loads an instance from the data file. When that instance is deleted, tries to find the old state
   * 
   */
  def getZombieInstanceData(ref:Reference):InstanceData = {
  	val handler=getHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance )
  	var pos=rec.dataPos
  	var length=rec.dataLength
  	if (rec.dataPos == -1 && rec.dataLength==0) { // deleted
  		val (transType,npos,nlength)=TransLogHandler.getLastLivingData(ref.typ,ref.instance,
  			TransLogHandler.insertPos-1,List(TransType.created.id,TransType.dataChanged.id))
  		//System.out.println("Last Living "+ref+" is: pos:"+npos+" size:"+nlength)
  		if(transType== -1) return new InstanceData(ref,IndexedSeq(),Array(),Seq.empty,false)
  		pos=npos;length=nlength
  		
  	}
  	//else if(rec.dataPos<1) System.out.println("get Inst pos<1 :"+ref+" "+rec.dataPos)
  	val instObj=dataFileHandler.readWithBool(ref,pos,length,false )
  	instObj
  }
  
  def pushInstanceData(ref:Reference,out:DataOutput) = {  	
  	val handler=getHandler(ref.typ)
  	//System.out.println("push before "+ref)
  	val rec=handler.getInstanceRecord(ref.instance )
  	//System.out.println("push "+ref+" "+rec)
  	ref.write(out)
  	dataFileHandler.pushData(rec.dataPos,rec.dataLength,out )
  	out.writeBoolean((rec.propPos !=0)&&(rec.propLength !=0))
  }
  
  
  def bulkGetInstanceData(startRef:Reference,endRef:Reference) = {
  	//System.out.println("bulkget:"+startRef+" - "+endRef)
  	getHandler(startRef.typ).bulkGetInstanceRecords(startRef.instance,endRef.instance,dataFileHandler)  	 
  }
  
  def bulkPushInstanceData(startRef:Reference,endRef:Reference,out:DataOutput) = {
  	//System.out.println("bulkpush:"+startRef+" - "+endRef)
  	getHandler(startRef.typ).bulkPushInstanceRecords(startRef.instance,endRef.instance,dataFileHandler,out)
  }
  
      
  /** creates a new empty instance
   * 
   */
  def createInstance(typ:Int,owners:Array[OwnerReference],withStartValues:Boolean):InstanceData =
  {
  	val hand =getHandler(typ)
  	val inst=hand.theClass.createInstance(
  		new Reference(typ,hand.createInstance()),owners,withStartValues)  	
  	//writeInstance(inst)
  	inst  
  }
  
  /*def getNextInstanceNr(typ:Int):Long =  {
  	getHandler(typ).lastID + 1L
  }*/
  
  /** writes an instance to the data file
   *  @param created was this instance created during this transaction and should a created
   *  record be stored in the transaction log 
   */
  def writeInstance(data:InstanceData,created:Boolean):Unit =  {  	
  	val (pos,size)=dataFileHandler.writeInstance(data)
  	val handler= getHandler(data.ref.typ)
  	handler.writeData(data.ref.instance, pos, size,created)
  	handler.instCache.putInstanceData(data)  	
  	//SimpleProfiler.measure("writeInst")
  }
  
  /** deletes an instace from the index
   * 
   */
  def deleteInstance(typ:Int,inst:Int) = 
  {
  	val handler=getHandler(typ)
  	handler.deleteInstance(inst)
  	handler.instCache.removeInstanceData(inst)
  	handler.propCache.removeInstanceData(inst)  	
  }
  
  /* creates an empty Properties object with default values
   * this object will not be stored with this function call
   */
  def createInstanceProperties(ref:Reference):InstanceProperties =  {
  	getHandler(ref.typ).theClass.createInstanceProperty(ref)
  }
  
  // *************************************************************************************
  //                                       PROPERTIES
  
  
  def getInstanceProperties(ref:Reference):Option[InstanceProperties] =  {
  	val handler=getHandler(ref.typ)
  	//System.out.println("get Inst prop "+ref)
  	handler.propCache.getInstanceData(ref.instance) match
  	{
  		case a @Some(_) => a
  		case None =>
  	  {
  	    val rec=handler.getInstanceRecord(ref.instance)
  	    //System.out.println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.propPos == 0 && rec.propLength==0) None
  	    else
  	    {
  	      val propObj=propFileHandler.readInstance(ref,rec.propPos,rec.propLength)
  	      handler.propCache.putInstanceData(propObj)
  	      Some(propObj)
  	    }
  	  }
  	}  	
  }
  
  def writeInstanceProperties(data:InstanceProperties)= {
  	val hasChildren=data.hasChildren
  	val (pos,size)= if(hasChildren)propFileHandler.writeInstance(data)
  									else (0L,0)// if there are no children, delete this property data set  	
  	
  	val handler= getHandler(data.ref.typ)
  	// check instanceData in instanceCache
  	handler.instCache.getInstanceData(data.ref.instance) match
  	{ // if it has no correct information about hasChildren, update it
  		case Some(a) => if(a.hasChildren!=hasChildren) handler.instCache.putInstanceData(a.setHasChildren(hasChildren))
  		case None => // not in cache, nothing to change
  	}
  	handler.writePropertiesData(data.ref.instance, pos, size)
  	handler.propCache.putInstanceData(data)
  	//SimpleProfiler.measure("wprop")
  }
  
  // *************************************************************************************
  //                                    L I N K S
  
  
  def getReferencingLinks(ref:Reference):Option[ReferencingLinks] = {
  	val handler=getHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance)
  	    //System.out.println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.linkPos == 0 && rec.linkLength==0) None
  	    else
  	    {
  	      val propObj=linkFileHandler.readInstance(ref,rec.linkPos,rec.linkLength )
  	      // not chached yet
  	      Some(propObj)
  	    }
  }
  
  
  def writeReferencingLinks(data:ReferencingLinks) = {
  	val (pos,size)=linkFileHandler.writeInstance(data)
  	val handler= getHandler(data.ref.typ)
  	handler.writeLinksData(data.ref.instance, pos, size)
  }
  
  
  // *************************************************************************************
  //                                   Collecting-Functions
  
  def getCollectingFuncData(ref:Reference):Option[CollFuncResultSet] = {
  	val handler=getHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance)
  	    //System.out.println(" collfunc recpos "+rec.collPos +" recsize:"+rec.collLength)
  	    if (rec.collPos == 0 && rec.collLength==0) None
  	    else
  	    {
  	      val propObj=collFuncFileHandler.readInstance(ref,rec.collPos,rec.collLength)
  	      // not chached yet
  	      Some(propObj)
  	    }
  }
  
  
  def writeCollectingFuncData(data:CollFuncResultSet) = {
  	val (pos,size)=collFuncFileHandler.writeInstance(data)
  	val handler= getHandler(data.ref.typ)
  	handler.writeCollFuncData(data.ref.instance, pos, size)
  }
  
  
  
  def shutDown() =
  {
  	if(!shuttedDown)
  	{
  		System.out.println("Shutdown Storage")
  		for (i <- ixHandlerList.valuesIterator) i.shutDown
  		dataFileHandler.shutDown
  		propFileHandler.shutDown
  		TransLogHandler.shutDown
  		TransDetailLogHandler.shutDown
  		linkFileHandler.shutDown
  		collFuncFileHandler.shutDown
  		shuttedDown=true
  		inited=false
  	}
  }
  
  def printCacheReport() = {
   for (i <- ixHandlerList.valuesIterator) i.printCaches	
  }
  
  def undoLastStep() = {
  	val currTrID=TransLogHandler.transID
  	val startLogPos=TransLogHandler.insertPos-1
  	var currLogPos=startLogPos
  	System.out.println("startUNDO insPos:"+TransLogHandler.insertPos )
  	var currRec=TransLogHandler.readPosition(currLogPos)
  	while(currLogPos>0 && currRec.trID ==currTrID)
  	{
  		print(currRec.toString)
  		undoTransRec(currRec,currLogPos)
  		currLogPos-=1
  		currRec=TransLogHandler.readPosition(currLogPos)
  	}
  	System.out.println()
  	TransLogHandler.undoLastStep(startLogPos-currLogPos)
  	TransDetailLogHandler.undoLastStep
  }
  
  private def undoTransRec(rec:LogIndexSet,currLogPos:Int) = {
  	//var ret:(Int,Long,Int)=null
  	
  	val handler=getHandler(rec.typ)
    rec.transTyp match {    	
    	case TransType.deleted|TransType.dataChanged => {
    		val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
    			undoDeleteList)
    		handler.writeData(rec.inst,pos, len, trans==TransType.created.id,false)	
    		handler.instCache.removeInstanceData(rec.inst)
    	}
    	case TransType.created =>  {
    		handler.deleteInstance(rec.inst,false)
    		handler.instCache.removeInstanceData(rec.inst)
    	}
    	case TransType.linksChanged => {
    		val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
    			undoLinkList)
    		if(trans==TransType.created.id) // no info since creation
    		  handler.writeLinksData(rec.inst,0,0,false)
    		else handler.writeLinksData(rec.inst,pos,len,false)
    	}
    	case TransType.propertyChanged => {
    		val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
    			undoPropList)
    		if(trans==TransType.created.id) // no info since creation
    		  handler.writePropertiesData(rec.inst,0,0,false)
    		else handler.writePropertiesData(rec.inst,pos,len,false)
    		handler.propCache.removeInstanceData(rec.inst)
    	}
    	case TransType.collFuncChanged => {
    		val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
    			undoCollList)
    		if(trans==TransType.created.id) // no info since creation
    		  handler.writeCollFuncData(rec.inst,0,0,false)
    		else handler.writeCollFuncData(rec.inst,pos,len,false)
    	}
    }
  }
  
	
}