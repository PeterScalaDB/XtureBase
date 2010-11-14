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
	val recordSize=25
	val bufferStream=(new MyByteStream(recordSize))
	val outStream=new DataOutputStream(bufferStream)
	//var nextUserID:Short=0	
	
	var readBuffer= new Array[Byte](recordSize)	
	var inBufferStream=new ByteArrayInputStream(readBuffer)
	var dataInStream=new DataInputStream(inBufferStream)
	
	var insertPos:Int=0
		
	var transID:Int =
		if(theFile.length==0) { theFile.writeInt(1); 1 } 
	  else {
	  	theFile.seek(0)
	  	insertPos=theFile.readInt
	  	println("TransLog insPos:"+insertPos)
	  	theFile.seek(getSeekPos(insertPos-1)+1)	  	
	  	theFile.readInt
	  }
	readFinished()  
	
	def getSeekPos(inPos:Int)= 4+inPos*recordSize
	
	def incrementTransID():Long = {				
		transID += 1;		
		transID 
	} 
	
	def resetTransID()= transID -=1
	
	def instanceCreated(ref: Reference ) =	{		
		dataChanged(TransType.created,ref.typ,ref.instance,0,0)		
	}	
	
	def dataChanged(transTyp: TransType.Value,typ:Int,inst:Int,dataPos:Long,dataLength:Int) =	{		
		//println("TransLog changed ID:" + transID+ " transtyp:"+transTyp+" "+typ+", "+inst )		
		bufferStream.reset()		
		outStream.writeByte(transTyp.id)
		outStream.writeInt(transID)
		//outStream.writeShort(nextUserID)
		outStream.writeInt(typ)		
		outStream.writeInt(inst )
		outStream.writeLong(dataPos)
		outStream.writeInt(dataLength)
		theFile.write(bufferStream.buffer,0,bufferStream.size())
		insertPos+=1
	}
	
	def shutDown() =	{
		theFile.seek(0)
		theFile.writeInt(insertPos)
		theFile.close
		//TimeLogger.theFile.close
	}
	
	def readFullIndex():Array[LogIndexSet] = {
		theFile.seek(4)
		var retList: List [LogIndexSet] =Nil
		while(theFile.getFilePointer < theFile.length) {
			theFile.read(readBuffer,0,recordSize)
			inBufferStream.reset
			retList= LogIndexSet(TransType(dataInStream.readByte),dataInStream.readInt,
				dataInStream.readInt,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt) :: retList
		}
		readFinished()	
		retList.reverse.toArray
	}
	
	
	
	def readPosition(pos:Int) = {
		theFile.seek(getSeekPos(pos))
		theFile.read(readBuffer,0,recordSize)
		inBufferStream.reset
		LogIndexSet(TransType(dataInStream.readByte),dataInStream.readInt,
				dataInStream.readInt,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
	}
	
	/** finds the last occurence of a certain data type of a certain instance
	 * @param typ the type of the instance
	 * @param inst the instance id of the instance
	 * @param startPos the position in the log where the seach should start backwards
	 * @param fittingTrActions TransType actions that we are looking for 
	 * @returns (transType.id,dataPos,dataLength)
	 */
	def getLastLivingData(typ:Int,inst:Int,startPos:Int,fittingTrActions:Seq[Int]):(Int,Long,Int)= {
		var currPosition=startPos
		while(currPosition>0){
			theFile.seek(4+currPosition*recordSize+5)
			val ttype=theFile.readInt
			val tinst=theFile.readInt
			if(ttype==typ&&tinst==inst ) {
				theFile.seek(4+currPosition*recordSize)
				val trans=theFile.readByte
				if(fittingTrActions.contains(trans)) {
				  theFile.readLong;theFile.readInt // skip next fields
				  val rpos=theFile.readLong
				  val rinst=theFile.readInt
				  readFinished()
				  return (trans,rpos,rinst)
				}			
			}
			currPosition-=1			
		}
		readFinished()
		return (-1,0L,0)
	}
	
	def readFinished() = {
		theFile.seek(getSeekPos(insertPos))
	}	
	
	/** notifies the Log Handler to reset the pointers behind the removed transaction
	 * @param num of transaction steps to be removed
	 */
	def undoLastStep(numStepsBack:Int) = {		
		insertPos -=numStepsBack
		println("translog undo "+insertPos)
		readFinished()
		transID -=1
	}
	
}
