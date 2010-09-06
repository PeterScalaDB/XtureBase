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
	
	
	
	/** Stores an Instance in the Data File
	 @param data Instance to Store
	 @return the position in the Data file and the num of bytes written 
	 */
	def writeInstance(data:T):(Long,Int) =
	{
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
		if(size>readBuffer.size) {
			readBuffer=new Array[Byte](size+128)
			inBufferStream=new ByteArrayInputStream(readBuffer)
			dataInStream=new DataInputStream(inBufferStream)
		}		
		
		theFile.seek(pos)
		theFile.read(readBuffer,0,size)
		inBufferStream.reset		
		factory(ref,dataInStream)
	}
	
	
	
	def shutDown()=
	{
		theFile.close
	}

}

class MyByteStream(nsize:Int) extends ByteArrayOutputStream(nsize) {
	def buffer=buf
}