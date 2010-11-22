/**
 * Author: Peter Started:25.07.2010
 */
package server.storage


import definition.typ._
import definition.data._
import server.config._
import java.io._

/** Manages the index file for a certain class
 * 
 */

class ClassIndexHandler(val theClass:ServerObjectClass)
{
	val fileName=new File(FSPaths.dataDir+theClass.name+".idx")	
	val theFile= new RandomAccessFile(fileName,"rwd") 
	final val recordSize=8*4+4*5
	var numRecords:Int=(theFile.length/recordSize).toInt
	//println("Typ: "+theClass.id+" numRecords:"+numRecords)
	val firstID= if(numRecords>0) readIxInst(0) else 0
	var lastID:Int= if(numRecords>0) readIxInst(numRecords-1) else 0
	
	
	var lastReadID:Int= -2	
	var lastSeekID:Int = -2
	var lastSeekPos:Option[Int] = None
	
	val bufferStream=new MyByteStream(recordSize)
	val miniBufferStream=new MyByteStream(12)
	val miniOutStream=new DataOutputStream(miniBufferStream)
	
	
	val outStream=new DataOutputStream(bufferStream)
	for(i <-0 until 6 )	outStream.writeLong(0)
	outStream.writeInt(0)
	
	var readBuffer= new Array[Byte](recordSize)
	var bulkReadBuffer=readBuffer
	var inBufferStream=new ByteArrayInputStream(readBuffer)
	var dataInStream=new DataInputStream(inBufferStream)
	
	val instCache=new Cache[InstanceData](theClass.id)
	val propCache=new Cache[InstanceProperties](theClass.id)
	
	var counter=0
	
	
	// stores Information about the instance data in the index
	
	def createInstance():Int =
	{
		val inst=lastID+1		
		theFile.seek(theFile.length)
		resetLastReadID
		bufferStream.reset()
		outStream.writeInt(inst)
		outStream.writeLong(0)
		outStream.writeInt(0)
		theFile.write(bufferStream.buffer,0,recordSize)
		numRecords+=1
		lastID=inst		
		inst
	}	
	
	def resetLastReadID=if(lastReadID!= -2) lastReadID= -2
	
	
	
	def writeData(inst:Int,dataPos:Long,dataLength:Int,created:Boolean,withLog:Boolean=true) =
	{	  
	  internalWrite(inst,dataPos,dataLength,0) 
	  if(withLog){
	  	if(created) TransLogHandler.dataChanged(TransType.created,theClass.id,inst,dataPos,dataLength) 
	  	else        TransLogHandler.dataChanged(TransType.dataChanged,theClass.id,inst,dataPos,dataLength)
	  }
	}
	
	
	
	
	private def internalWrite(inst:Int,dataPos:Long,dataLength:Int,
	                          ixOffset:Int ) = // Offset position in the index record
	{
		if(inst>lastID)
	  { // create new Instance
	  	throw new IllegalArgumentException("Storing wrong instance" +inst+ " in class "+theClass.name)
	  }
		theFile.seek(findIxRecord(inst)*recordSize+4+ixOffset)
		resetLastReadID
		miniBufferStream.reset()
		miniOutStream.writeLong(dataPos)
	  miniOutStream.writeInt(dataLength)
	  theFile.write(miniBufferStream.buffer,0,12)
	}
	
	def writePropertiesData(inst:Int,dataPos:Long,dataLength:Int,withLog:Boolean=true) =
	{
    internalWrite(inst,dataPos,dataLength,8+4)
    if(withLog)TransLogHandler.dataChanged(TransType.propertyChanged ,theClass.id,inst,dataPos,dataLength)
	}
	
	def writeLinksData(inst:Int,dataPos:Long,dataLength:Int,withLog:Boolean=true) =
	{
    internalWrite(inst,dataPos,dataLength,(8+4)*2)
    if(withLog)TransLogHandler.dataChanged(TransType.linksChanged ,theClass.id,inst,dataPos,dataLength)
	}
	
	def writeCollFuncData(inst:Int,dataPos:Long,dataLength:Int,withLog:Boolean=true) =
	{
		internalWrite(inst,dataPos,dataLength,(8+4)*3)
		if(withLog)TransLogHandler.dataChanged(TransType.collFuncChanged ,theClass.id,inst,dataPos,dataLength)
	}
	
	
	
	
	
	
	
	def deleteInstance(inst:Int,withLog:Boolean=true) =
	{
		internalWrite(inst,-1,0,0)
		if(withLog)TransLogHandler.dataChanged(TransType.deleted,theClass.id,inst,0,0)
	}
	
	
	// does the specified instance exist ?
	def instanceExists(inst:Int):Boolean =
	{
		resetLastReadID
		for(i<-internFindIxRecord(inst))
				{
			     val r=getInstanceRecord(inst)
			     return (r.dataPos!= -1 && r.dataLength!=0) 
				}
		false
	}
	
	
	
	
	def shutDown() =
	{
		theFile.close
	}
	
	def bulkGetInstanceRecords(startInst:Int,endInst:Int,dataFileHandler:BoolContFileHandler[InstanceData])=  {
		
		if (bulkReadBuffer.size<(endInst-startInst+1)*recordSize) 
			bulkReadBuffer=new Array[Byte](recordSize*(endInst-startInst+1).toInt)			
		
		theFile.seek(findIxRecord(startInst)*recordSize)
		theFile.read(bulkReadBuffer,0,recordSize*(endInst-startInst+1).toInt)
		//println("buffer read "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for(i<-startInst to endInst;val offset=(i-startInst).toInt*recordSize+4) 			
			yield	dataFileHandler.readWithBool(Reference(theClass.id,i),getLong(bulkReadBuffer,offset),
				getInt(bulkReadBuffer,offset+8),getLong(bulkReadBuffer,offset+8+4)!=0 	)				
	}
	
	
	def bulkPushInstanceRecords(startInst:Int,endInst:Int,dataFileHandler:BoolContFileHandler[InstanceData],
	                           out:DataOutput)=  {
		if (bulkReadBuffer.size<(endInst-startInst+1)*recordSize) 
			bulkReadBuffer=new Array[Byte](recordSize*(endInst-startInst+1).toInt)			
		
		theFile.seek(findIxRecord(startInst)*recordSize)
		theFile.read(bulkReadBuffer,0,recordSize*(endInst-startInst+1).toInt)
		//println("buffer push "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for(i<-startInst to endInst){
			val offset=(i-startInst).toInt*recordSize+4
			out.writeInt(theClass.id) // reference
			out.writeInt(i)
			dataFileHandler.pushData(getLong(bulkReadBuffer,offset),
				getInt(bulkReadBuffer,offset+8) ,out	)
			out.writeBoolean(getLong(bulkReadBuffer,offset+8+4)!=0)
		}
			
	}
	
	
	def getLong(readBuffer:Array[Byte],pos:Int):Long = 
	 ((readBuffer(pos).toLong << 56) +
    ((readBuffer(pos+1) & 255).toLong << 48) +
    ((readBuffer(pos+2) & 255).toLong << 40) +
    ((readBuffer(pos+3) & 255).toLong << 32) +
    ((readBuffer(pos+4) & 255).toLong << 24) +
    ((readBuffer(pos+5) & 255).toLong << 16) +
    ((readBuffer(pos+6) & 255).toLong << 8) +
    ((readBuffer(pos+7) & 255).toLong << 0) )
    
		
	def getInt (readBuffer:Array[Byte],pos:Int):Int =
		((readBuffer(pos+0) & 255).toInt << 24) +
    ((readBuffer(pos+1) & 255).toInt << 16) +
    ((readBuffer(pos+2) & 255).toInt << 8) +
    ((readBuffer(pos+3) & 255).toInt << 0) 
	
	
	 def getInstanceRecord (inst:Int):IndexRecord =
	{	
		 //print("get")
		if(lastReadID!=(inst-1)) // optimize: if this call is the subsequent inst of the last call, try not to seek
			theFile.seek(findIxRecord(inst)*recordSize)
		//else print("o ")
		lastReadID=inst
		theFile.read(readBuffer,0,recordSize)
		inBufferStream.reset		
		new IndexRecord(dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,
			dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
	}
	
	
	 def readFully():Array[IndexRecord] = {
		 val retArray=new Array[IndexRecord](numRecords.toInt)
		 theFile.seek(0)
		 for(i<- 0 until numRecords) {
			 theFile.read(readBuffer,0,recordSize)
			 inBufferStream.reset		
			 retArray(i)=new IndexRecord(dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,
				 dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)			 
		 }
		 retArray
			 
	 }
	
	// internal routines
	
	// gets the Position of the given Instance in the index file
	private def internFindIxRecord(inst:Int):Option[Int] =
	{
		if(inst==lastSeekID) lastSeekPos
		else {			
		  if(numRecords==0) None
		  if(inst==lastID) numRecords
		  lastSeekID=inst
		  //if(inst==firstID) 0		
		  // binary search
		  def finder(lower:Int,upper:Int):Option[Int] =
		  {
		  	if (upper<lower) None
		  	val mid = (upper + lower) / 2
		  	val instAtMid=readIxInst(mid)
		  	if (instAtMid > inst)
		  		finder(lower, mid-1)
		  		else if (instAtMid < inst)
		  			finder( mid+1, upper)
		  			else
		  				Some(mid)
		  }

		  lastSeekPos=finder(0,numRecords)
		  lastSeekPos
		}
				
	}
	
	private def findIxRecord(inst:Int):Int =
	{
		resetLastReadID
		internFindIxRecord(inst) match 
		{ 
			case Some(v)=>v ;
			case None => throw new IllegalArgumentException("Instance "+inst+" not found in class "+theClass.name )
		}
	}
	
	// reads the Instance # at the given position 
	private def readIxInst(pos:Int) =
	{
		//println("readIxInst "+counter)
		//counter+=1
		theFile.seek(pos*recordSize)
		theFile.readInt
	}
	
	def printCaches = {
		println(instCache)
		println(propCache)
	}
}



/** Index record for instance data
 * 
 */
class IndexRecord (val inst:Int,val dataPos:Long,val dataLength:Int,val propPos:Long,val propLength:Int,
	val linkPos:Long,val linkLength:Int,val collPos:Long,val collLength:Int)
{

}