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

class ClassIndexHandler(val theClass:ObjectClass)
{
	val fileName=new File(FSPaths.dataDir+theClass.name+".idx")	
	val theFile= new RandomAccessFile(fileName,"rwd") 
	val recordSize=8*4+4*3
	var numRecords=theFile.length/recordSize
	//println("Typ: "+theClass.id+" numRecords:"+numRecords)
	val firstID= if(numRecords>0) readIxInst(0) else 0
	var lastID:Long= if(numRecords>0) readIxInst(numRecords-1) else 0
	
	val instCache=new Cache[InstanceData](theClass.id)
	val propCache=new Cache[InstanceProperties](theClass.id)
	
	
	// stores Information about the instance data in the index
	
	def writeData(inst:Long,dataPos:Long,dataLength:Int) =
	{	  
	  internalWrite(inst,dataPos,dataLength,0) 
	  TransLogHandler.dataChanged(TransType.dataChanged,theClass.id,inst,dataPos,dataLength)
	}
	
	
	
	
	private def internalWrite(inst:Long,dataPos:Long,dataLength:Int,
	                          ixOffset:Int ) = // Offset position in the index record
	{
		if(inst>lastID)
	  { // create new Instance
	  	throw new IllegalArgumentException("Storing wrong instance" +inst+ " in class "+theClass.name)
	  }
		theFile.seek(findIxRecord(inst)*recordSize+8+ixOffset)
		theFile.writeLong(dataPos)
	  theFile.writeInt(dataLength)
	}
	
	def writePropertiesData(inst:Long,dataPos:Long,dataLength:Int) =
	{
    internalWrite(inst,dataPos,dataLength,8+4)
    TransLogHandler.dataChanged(TransType.propertyChanged ,theClass.id,inst,dataPos,dataLength)
	}
	
	def writeLinksData(inst:Long,dataPos:Long,dataLength:Int) =
	{
    internalWrite(inst,dataPos,dataLength,(8+4)*2)
    TransLogHandler.dataChanged(TransType.linksChanged ,theClass.id,inst,dataPos,dataLength)
	}
	
	
	def createInstance():Long =
	{
		val inst=lastID+1		
		theFile.seek(theFile.length)
		theFile.writeLong(inst)
		theFile.writeLong(0);theFile.writeInt(0)
		theFile.writeLong(0);theFile.writeInt(0)
		theFile.writeLong(0);theFile.writeInt(0)
		numRecords+=1
		lastID=inst
		TransLogHandler.dataChanged(TransType.created,theClass.id,inst,0,0)
		inst
	}
	
	
	
	
	def deleteInstance(inst:Long) =
	{
		internalWrite(inst,0,0,0)
		TransLogHandler.dataChanged(TransType.deleted,theClass.id,inst,0,0)
	}
	
	
	// does the specified instance exist ?
	def instanceExists(inst:Long):Boolean =
	{
		for(i<-internFindIxRecord(inst))
				{
			     val r=getInstanceRecord(inst)
			     return (r.dataPos!=0 && r.dataLength!=0) 
				}
		false
	}
	
	
	
	
	def shutDown() =
	{
		theFile.close
	}
	
	
	 def getInstanceRecord (inst:Long):IndexRecord =
	{		
		theFile.seek(findIxRecord(inst)*recordSize)
		new IndexRecord(theFile.readLong,theFile.readLong,theFile.readInt,theFile.readLong,theFile.readInt,
			theFile.readLong,theFile.readInt)
	}
	
	// internal routines
	
	// gets the Position of the given Instance in the index file
	private def internFindIxRecord(inst:Long):Option[Long] =
	{
		if(numRecords==0) None
		if(inst==lastID) numRecords
		if(inst==firstID) 0		
		// binary search
		def finder(lower:Long,upper:Long):Option[Long] =
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
		
		finder(0,numRecords)		
	}
	
	private def findIxRecord(inst:Long):Long =
	{
		internFindIxRecord(inst) match 
		{ 
			case Some(v)=>v ;
			case None => throw new IllegalArgumentException("Instance "+inst+" not found in class "+theClass.name )
		}
	}
	
	// reads the Instance # at the given position 
	private def readIxInst(pos:Long) =
	{
		theFile.seek(pos*recordSize)
		theFile.readLong
	}
	
	def printCaches = {
		println(instCache)
		println(propCache)
	}
}



/** Index record for instance data
 * 
 */
class IndexRecord (val inst:Long,val dataPos:Long,val dataLength:Int,val propPos:Long,val propLength:Int,val linkPos:Long,val linkLength:Int)
{

}