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
	
	
	/** Stores an Instance in the Data File
	 @param data Instance to Store
	 @return the position in the Data file and the num of bytes written 
	 */
	def writeInstance(data:T):(Long,Int) =
	{
		val pos=theFile.length
		theFile.seek(pos)
		data.write(theFile)
		val numBytes:Int=(theFile.length-pos).toInt
		(pos,numBytes)
	}
	
	/**
	 *  reads one instance out of the data File
	 */
	def readInstance(ref:Reference,pos:Long):T =
	{
		theFile.seek(pos)
		factory(ref,theFile)
	}
	
	
	
	def shutDown()=
	{
		theFile.close
	}

}