/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import java.io.{DataInput,DataOutput}

/** a reference to an instance
 * 
 */
case class Reference (typ:Int, instance:Long) 
{
   def sToString() = "["+typ+", "+instance+"]"
   
   def write(file: DataOutput ) = { 
  	 file.writeInt(typ);file.writeLong(instance)
   }
}

object Reference
{
	def apply(file: DataInput) =
	{
	   new Reference(file.readInt,file.readLong)	
	}
}

