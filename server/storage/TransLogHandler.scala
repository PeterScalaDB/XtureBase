/**
 * Author: Peter Started:26.07.2010
 */
package server.storage

import definition.data._
import server.config._
import java.io._

/** manages the transaction log
 * 
 */
object TransLogHandler 
{
	val fileName=FSPaths.dataDir+"transaction.log"	
	val theFile= new RandomAccessFile(fileName,"rwd")
	var transID:Long =
		if(theFile.length==0) { theFile.writeLong(1); 1 } 
	  else theFile.readLong;
	
	theFile.seek(theFile.length)
  
	
	def incrementTransID():Long = {transID += 1; transID } 
	
	def resetTransID()= transID -=1
	
	def instanceCreated(ref: Reference ) =
	{		
		println("Translog created ID:"+transID+" " +ref)
		theFile.writeByte(TransType.created.id)
		theFile.writeLong(transID)
		theFile.writeInt(ref.typ)		
		theFile.writeLong(ref.instance )
		theFile.writeLong(0)
		theFile.writeInt(0)
	}
	
	
	
	def dataChanged(transTyp: TransType.Value,typ:Int,inst:Long,dataPos:Long,dataLength:Int) =
	{		
		println("TransLog changed ID:" + transID+ " transtyp:"+transTyp+" "+typ+", "+inst )
		theFile.writeByte(transTyp.id)
		theFile.writeLong(transID)
		theFile.writeInt(typ)		
		theFile.writeLong(inst )
		theFile.writeLong(dataPos)
		theFile.writeInt(dataLength)
	}
	
	def shutDown() =
	{
		theFile.seek(0)
		theFile.writeLong(transID)
		theFile.close
	}
	
	def readFullIndex():Array[(Byte,Long,Int,Long,Long,Int)] = {
		theFile.seek(8)
		var retList: List [(Byte,Long,Int,Long,Long,Int)] =Nil
		while(theFile.getFilePointer < theFile.length)
			retList= (theFile.readByte,theFile.readLong,theFile.readInt,theFile.readLong,theFile.readLong,theFile.readInt) :: retList
		retList.reverse.toArray
	}
	
	
}


/**
 *  defines the transaction types
 */
object TransType extends Enumeration 
{	
	val created=Value("created")
	val dataChanged=Value("Data changed")
	val propertyChanged=Value("Prop changed")
	val linksChanged=Value("Links changed")
	val deleted=Value("deleted")
}