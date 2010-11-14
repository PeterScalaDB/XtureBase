/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import java.io.{DataInput,DataOutput,Serializable}
import definition.typ.AllClasses

/** a reference to an instance
 * 
 */
case class Reference (typ:Int, instance:Int) extends Referencable
{
   def sToString() = "("+typ+", "+instance+")"
   
   def lToString() = "("+AllClasses.get.getClassByID(typ).name+" #"+instance+")"
    
   override def write(file: DataOutput ) = { 
  	 file.writeInt(typ);file.writeInt(instance)
   }
   
   def isNull=  typ==0 && instance==0
   
   def serialized= new SerialReference(typ,instance)
   def ref= this
}

@SerialVersionUID(4276L)
case class SerialReference extends Serializable {
	
	def this(ntyp:Int,ninst:Int) = {
		this()
		typ=ntyp
		instance=ninst
	}
	var typ:Int=0
	var instance:Int=0
	
	override def toString = "("+typ+","+instance+")"
	
	def toReference=new Reference(typ,instance)
	
	override def equals(other: Any): Boolean =
		other match {
				case that: SerialReference =>
				(that canEqual this) && typ==that.typ && instance== that.instance				
				case that: Reference => typ==that.typ && instance== that.instance
				case _ => false
		}
	
	def canEqual(other: Any): Boolean = other.isInstanceOf[SerialReference] ||
			other.isInstanceOf[Reference]
	
	override def hashCode: Int =  41 * ( 41 + typ ) + instance
}

object Reference
{
	def apply(file: DataInput) =
	{
	   new Reference(file.readInt,file.readInt)	
	}
}

