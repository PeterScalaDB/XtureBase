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
	val bufferStream=(new MyByteStream(34))
	val outStream=new DataOutputStream(bufferStream)
	
		
	var transID:Long =
		if(theFile.length==0) { theFile.writeLong(1); 1 } 
	  else theFile.readLong;	
	theFile.seek(theFile.length)
  
	
	def incrementTransID():Long = {transID += 1; transID } 
	
	def resetTransID()= transID -=1
	
	def instanceCreated(ref: Reference ) =
	{		
		//println("Translog created ID:"+transID+" " +ref)
		dataChanged(TransType.created,ref.typ,ref.instance,0,0)		
	}
	
	
	
	def dataChanged(transTyp: TransType.Value,typ:Int,inst:Long,dataPos:Long,dataLength:Int) =
	{		
		//println("TransLog changed ID:" + transID+ " transtyp:"+transTyp+" "+typ+", "+inst )		
		bufferStream.reset()		
		outStream.writeByte(transTyp.id)
		outStream.writeLong(transID)
		outStream.writeInt(typ)		
		outStream.writeLong(inst )
		outStream.writeLong(dataPos)
		outStream.writeInt(dataLength)
		theFile.write(bufferStream.buffer,0,bufferStream.size())
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
	val collFuncChanged=Value("CollFunc changed")
	val deleted=Value("deleted")	
}