/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import definition.typ._
import definition.data._
import java.util.NoSuchElementException
import java.io.DataInput
import runtime.function._
import definition.expression.FunctionManager


/** manages all file io operations
 * 
 */
object StorageManager {
  var ixHandlerList:Map[Int,ClassIndexHandler]=Map()
  val dataFileHandler=new ContainerFileHandler("InstData.dat",InstanceData.read)
  val propFileHandler=new ContainerFileHandler("PropData.dat",InstanceProperties.read)
  val linkFileHandler=new ContainerFileHandler("ExternLinks.dat",ReferencingLinks.read)
  val collFuncFileHandler=new ContainerFileHandler("collFuncs.dat",CollFuncResultSet.read)
  var shuttedDown=false
  var inited=false
  
  def init(classList:Map[Int,ObjectClass] ) =
  {
  	if(ixHandlerList.isEmpty||shuttedDown)
  	{
  		shuttedDown=false
  	  ixHandlerList=ixHandlerList++( for (i <-classList.valuesIterator) yield (i.id -> new ClassIndexHandler(i)))
  	}
  	FunctionManager.setManager(StorageFuncMan)
  }
  
  def getHandler(typ:Int) = try 
  { ixHandlerList(typ) } catch {case e:NoSuchElementException => throw new IllegalArgumentException("Storage: Type "+typ+" not found")}
  
  
  def instanceExists(typ:Int,inst:Long) =getHandler(typ).instanceExists(inst)
  
  
  /** loads an instance from the data file
   * 
   */
  def getInstanceData(ref:Reference):InstanceData =
 	{
  	//print("getInstance "+ref)
  	val handler=getHandler(ref.typ)
  	handler.instCache.getInstanceData(ref.instance ) match
  	{
  		case Some(cacheValue) =>{/*println(" cacheHit:"+cacheValue);*/ cacheValue}
  		case None =>
  	  {
  	    val rec=handler.getInstanceRecord(ref.instance )
  	    if (rec.dataPos == 0 && rec.dataLength==0) throw new 
  	               IllegalArgumentException("get Instance() instance "+ref+" is deleted")
  	    val instObj=dataFileHandler.readInstance(ref,rec.dataPos)
  	    //println(" cacheMiss:"+instObj)
  	    handler.instCache.putInstanceData(instObj)
  	    instObj
  	  }
  	}
  }  
  
      
  /** creates a new empty instance
   * 
   */
  def createInstance(typ:Int,owners:Array[OwnerReference]):InstanceData =
  {
  	val hand =getHandler(typ)
  	val inst=hand.theClass.createInstance(
  		new Reference(typ,hand.createInstance()),owners)  	
  	writeInstance(inst)
  	inst
  }
  
  /*def getNextInstanceNr(typ:Int):Long =  {
  	getHandler(typ).lastID + 1L
  }*/
  
  /** writes an instance to the data file
   * 
   */
  def writeInstance(data:InstanceData)=  {
  	val (pos,size)=dataFileHandler.writeInstance(data)
  	val handler= getHandler(data.ref.typ)
  	handler.writeData(data.ref.instance, pos, size)
  	handler.instCache.putInstanceData(data)  	
  }
  
  /** deletes an instace from the index
   * 
   */
  def deleteInstance(typ:Int,inst:Long) = 
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
  	//println("get Inst prop "+ref)
  	handler.propCache.getInstanceData(ref.instance) match
  	{
  		case a @Some(_) => a
  		case None =>
  	  {
  	    val rec=handler.getInstanceRecord(ref.instance)
  	    //println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.propPos == 0 && rec.propLength==0) None
  	    else
  	    {
  	      val propObj=propFileHandler.readInstance(ref,rec.propPos)
  	      handler.propCache.putInstanceData(propObj)
  	      Some(propObj)
  	    }
  	  }
  	}  	
  }
  
  def writeInstanceProperties(data:InstanceProperties)= {
  	val (pos,size)=propFileHandler.writeInstance(data)
  	//println("Write prop Pos:"+pos+ " Size:"+size)
  	val handler= getHandler(data.ref.typ)
  	handler.writePropertiesData(data.ref.instance, pos, size)
  	handler.propCache.putInstanceData(data)
  }
  
  // *************************************************************************************
  //                                    L I N K S
  
  
  def getReferencingLinks(ref:Reference):Option[ReferencingLinks] = {
  	val handler=getHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance)
  	    //println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.linkPos == 0 && rec.linkLength==0) None
  	    else
  	    {
  	      val propObj=linkFileHandler.readInstance(ref,rec.linkPos)
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
  	    println(" collfunc recpos "+rec.collPos +" recsize:"+rec.collLength)
  	    if (rec.collPos == 0 && rec.collLength==0) None
  	    else
  	    {
  	      val propObj=collFuncFileHandler.readInstance(ref,rec.collPos)
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
  		println("Shutdown Storage")
  		for (i <- ixHandlerList.valuesIterator) i.shutDown
  		dataFileHandler.shutDown
  		propFileHandler.shutDown
  		TransLogHandler.shutDown
  		linkFileHandler.shutDown
  		collFuncFileHandler.shutDown
  		shuttedDown=true
  		inited=false
  	}
  }
  
  def printCacheReport() = {
   for (i <- ixHandlerList.valuesIterator) i.printCaches	
  }
  
	
}