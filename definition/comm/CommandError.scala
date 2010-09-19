/**
 * Author: Peter Started:05.09.2010
 */
package definition.comm

import java.io._

/** the result of a database command
 * 
 */
case class CommandError(name:String,operationID:Int,reasonID:Int) extends Exception(name) {
	
	def write(out:DataOutput) = {		
		out.writeUTF(if(name==null) "" else name)
		out.writeInt(operationID)
		out.writeInt(reasonID)
	}

}

object CommandError {
	def read(in:DataInput) = {
		new CommandError(in.readUTF(),in.readInt,in.readInt)
	}
}