/**
 * Author: Peter Started:01.11.2010
 */
package server.storage

import server.config._
import java.io._
import definition.data.{Reference,TransStepData}


/** stores a hourly timestamp for the transaction log
 * if a new transaction was made more than 1 hour later than the last 
 * time stamp info, a new time stamp info is made.
 * So it is possible to reveal the working date/hours of transactions
 * 
 */
object TransDetailLogHandler {
	//var lastLoggedTime:Int=0
	var lastLoggedID:Int= -1
	val fileName=FSPaths.dataDir+"TransDetailIndex.log"
	val theFile= new RandomAccessFile(fileName,"rwd")
	var insertPos:Int =0
	val recordSize=27
	
	if(theFile.length>0) {
		insertPos=theFile.readInt
		theFile.seek(filePosToIndex(insertPos-1))
		lastLoggedID=theFile.readInt
		System.out.println("DetailLog last logged ID:"+lastLoggedID)
		theFile.seek(filePosToIndex(insertPos))
		//System.out.println("Last logged Time:"+new java.util.Date(lastLoggedTime*60000L)+" id:"+lastLoggedID)
	} else theFile.writeInt(0)	
	
	
	def filePosToIndex(pos:Int) = 4+pos*recordSize 
	
	def log(trID:Int,userID:Int,firstInst:Reference,multiInst:Boolean,action:Short,createType:Int ) = {		
		val time=(System.currentTimeMillis/60000).toInt
		lastLoggedID=trID
		//System.out.println("timediff:"+(time-lastLoggedTime))
	  theFile.writeInt(trID)
	  theFile.writeInt(time)
	  theFile.writeInt(userID)
	  theFile.writeInt(firstInst.typ)
	  theFile.writeInt(firstInst.instance)
	  theFile.writeBoolean(multiInst)
	  theFile.writeShort(action)
	  theFile.writeInt(createType)
	  insertPos +=1
	}
	
	def shutDown() = {
		theFile.seek(0)
		theFile.writeInt(insertPos)
	}
	
	def readTransStepData(trID:Int):TransStepData = {
		if (trID>lastLoggedID){
			System.out.println("TRID>lastLoggedID  trID:"+trID+" lastLoggedID"+lastLoggedID)
			null
		}
		else if(trID<0) {
			System.out.println("trid<0  "+trID)
			null
		}
		else {
			//System.out.println("read transStep "+trID+" insPos:"+insertPos+" lastLoggedID:"+lastLoggedID)
		  theFile.seek(filePosToIndex(insertPos-(lastLoggedID-trID)-1))		  
		  val ntrID=theFile.readInt
		  if(ntrID!=trID) System.err.println("readTransStep trID:"+trID+" ntrID:"+ntrID)
		  val time=theFile.readInt
		  val userID=UserList.getUserName(theFile.readInt)
		  val ref=Reference(theFile.readInt,theFile.readInt)
		  val multi=theFile.readBoolean
		  val action=ActionNameMap.getActionName(theFile.readShort)
		  val ct=theFile.readInt
		  new TransStepData(ntrID,time,userID,StorageManager.getZombieInstanceData(ref),multi,action,ct)	
		}		
	}
	
	def readTransStepData(fromID:Int,toID:Int):Seq[TransStepData] = {
		val ret=for(id <-fromID to toID by -1)
			yield readTransStepData(id)
		readFinished()
		ret
	}
	
	private def readFinished() = {
		theFile.seek(filePosToIndex(insertPos))
	}
	
	def readFully = {
		theFile.seek(4)		
		val ret=for (i <-0 until ((theFile.length-4)/recordSize).toInt) 
			yield	readTransStepData(i)
		readFinished()
		ret
	}	
	
	def undoLastStep= {
	  insertPos -=1
	  lastLoggedID-=1
	  readFinished()	
	}
	
	
	
		
}