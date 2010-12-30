/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import definition.typ._
import definition.data._
import server.config._
import java.io._

/** manages file access to the data file
 * 
 */
class ContainerFileHandler [T <: Referencable] (val fileName:String,factory: (Reference,DataInput) => T)
{	
	val theFile= new RandomAccessFile(FSPaths.dataDir+fileName,"rwd")	
	val bufferStream=(new MyByteStream(256))
	val outStream=new DataOutputStream(bufferStream)
	
	var readBuffer= new Array[Byte](256)
	var inBufferStream=new ByteArrayInputStream(readBuffer)
	var dataInStream=new DataInputStream(inBufferStream)
	
	var lastReadPos:Long= -2 
	var lastReadSize:Int= -2
	//var followCount=0
	
	/** Stores an Instance in the Data File
	 @param data Instance to Store
	 @return the position in the Data file and the num of bytes written 
	 */
	def writeInstance(data:T):(Long,Int) =
	{
		if(lastReadPos!= -2){			
			lastReadPos= -2 // reset cache marker
		}
		bufferStream.reset()		
		val pos=theFile.length
		theFile.seek(pos)
		
		data.write(outStream)
		theFile.write(bufferStream.buffer,0,bufferStream.size())		
		(pos,bufferStream.size())
	}
	
	/**
	 *  reads one instance out of the data File
	 */
	def readInstance(ref:Reference,pos:Long,size:Int):T =
	{
		readInBuffer(pos,size)
		factory(ref,dataInStream)
	}
	
	protected def readInBuffer(pos:Long,size:Int) = {
		if(size>readBuffer.size) {
			readBuffer=new Array[Byte](size+128)
			inBufferStream=new ByteArrayInputStream(readBuffer)
			dataInStream=new DataInputStream(inBufferStream)
		}	
		//print(" R:"+pos+","+size )
		if(pos!=lastReadPos+lastReadSize) { // dont seek for subsequent instances
			/*if(followCount>0) {				
				System.out.println("following hits:"+followCount)
				followCount=0
			}*/
			theFile.seek(pos)
		}
		//else { followCount+=1}
		//else print("h ")
		theFile.read(readBuffer,0,size)
		inBufferStream.reset
		lastReadPos=pos
		lastReadSize=size
	}	
	
		
	def shutDown()=
	{
		theFile.close
	}

}

class BoolContFileHandler [T <: Referencable] (override val fileName:String,withBoolFactory: (Reference,DataInput,Boolean) => T) extends
	  ContainerFileHandler[T](fileName,null)	
	{
		def readWithBool(ref:Reference,pos:Long,size:Int,boolValue:Boolean):T = {
			readInBuffer(pos,size)
			withBoolFactory(ref,dataInStream,boolValue)
		}		
		def pushData(pos:Long,size:Int,out:DataOutput) = {
			readInBuffer(pos,size)
			//System.out.println("pushData "+fileName)
			out.write(readBuffer,0,size)			
		}
	}


class MyByteStream(nsize:Int) extends ByteArrayOutputStream(nsize) {
	def buffer=buf
}