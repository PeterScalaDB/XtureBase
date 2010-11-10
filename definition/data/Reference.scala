/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import java.io.{DataInput,DataOutput}
import definition.typ.AllClasses

/** a reference to an instance
 * 
 */
case class Reference (typ:Int, instance:Int) 
{
   def sToString() = "("+typ+", "+instance+")"
   
   def lToString() = "("+AllClasses.get.getClassByID(typ).name+" #"+instance+")"
    
   def write(file: DataOutput ) = { 
  	 file.writeInt(typ);file.writeInt(instance)
   }
   
   def isNull=  typ==0 && instance==0
}

object Reference
{
	def apply(file: DataInput) =
	{
	   new Reference(file.readInt,file.readInt)	
	}
}

