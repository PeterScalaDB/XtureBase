/**
 * Author: Peter Started:01.11.2010
 */
package definition.data

import collection.mutable._
import java.io._


case class LogIndexSet(transTyp: TransType.Value,trID:Int,typ:Int,inst:Int,dataPos:Long,dataLength:Int) {
	override def toString= transTyp+" ["+typ+","+inst+"] "
}

case class TransStepData(val trID:Int,val time:Int,userID:String,firstInst:InstanceData,multiInst:Boolean,action:String,
	createType:Int) {
	//println("transstep "+trID)
	def write(out:DataOutput) = {
		out.writeInt(trID)
	  out.writeInt(time)
	  out.writeUTF(userID)
	  if(firstInst==null) Reference(0,0).write(out)
	  else {
	  	firstInst.ref.write(out)
	  	firstInst.write(out)
	  }
	  	  
	  out.writeBoolean(multiInst)
	  out.writeUTF(action)
	  out.writeInt(createType)
	}
}

object TransStepData {
	def read(in:DataInput)= {
		new TransStepData(in.readInt,in.readInt,in.readUTF,InstanceData.read(Reference(in.readInt,in.readInt),in,true),
			in.readBoolean,in.readUTF,in.readInt)		
	}
}
	

/**
 *  defines the transaction types
 */
object TransType extends Enumeration 
{	
	type TransType = Value
	val created=Value("created")
	val dataChanged=Value("Data changed")
	val propertyChanged=Value("Prop changed")
	val linksChanged=Value("Links changed")
	val collFuncChanged=Value("CollFunc changed")
	val deleted=Value("deleted")	
}
