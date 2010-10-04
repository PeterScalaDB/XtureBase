/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import definition.expression._
import java.io.{DataInput,DataOutput}
import definition.typ._

/** Holds the data of a certain Instance
 * 
 * 
 */
class InstanceData (override val ref:Reference,	val fieldData:Array[Expression],	 									
	val owners:Array[OwnerReference]=Array(),val hasChildren:Boolean) extends Referencable
	{	
	private var fieldValuesCache:Array[Constant]=new Array(fieldData.length)


override def toString() = "inst" + ref.sToString+ "("+ fieldData.mkString(", ")+ ")"


override def write(file:DataOutput) = 	{
	//ref.write(file)
	
	file.writeByte(fieldData.length)
	for(field<-fieldData)
	{			
		field.write(file)
	}

	file.writeByte(owners.length)
	for(owner<-owners)
		owner.write(file)	
}
	
 def writeWithChildInfo(file:DataOutput) = {
	write(file)
	file.writeBoolean(hasChildren)
}

/** changes the value of a single field and returns a new instance object with the new value
 *  
 * @param fieldNr number of the field to change
 * @param newValue new expression value
 * @return a new instance object with the new value
 */
def setField(fieldNr:Byte,newValue:Expression):InstanceData = 	{
	val newArray:Array[Expression]=(for (i <- 0 until fieldData.length) 
		yield (if (i==fieldNr) newValue else fieldData(i))).toArray
		new InstanceData(ref,newArray,owners,hasChildren)
}

def changeOwner(newOwners:Array[OwnerReference]) = {
	new InstanceData(ref,fieldData,newOwners,hasChildren)
}

def setHasChildren(newValue:Boolean) = {
	new InstanceData(ref,fieldData,owners,newValue)
}

/** creates a copy of this instance
 * a helper routine for copying instances.
 * At the moment it creates a clone with the same class version.
 * There could be another version converting an instance from an old version to a new one,
 * having a translation list for fields.
 * 
 * @param newRef the reference of the clone instance
 * @param newOwners the owners of the new instance
 * @return
 */
def clone(newRef:Reference,newOwners:Array[OwnerReference]):InstanceData =	{
	new InstanceData(newRef,fieldData,newOwners,hasChildren)
}

/** replaces an ownerReferene with another ref and returns a new Instance with the new values
 * 
 * @param fromRef the old ref to remove
 * @param toRef the new ref that replaces the old one
 */
def changeSingleOwner(fromRef:OwnerReference,toRef:OwnerReference):InstanceData = {
	val newOwnerList= for (ref <- owners)
		yield (if (ref==fromRef) toRef else ref)
		new InstanceData(ref,fieldData,newOwnerList,hasChildren)
}


def getObjectClass():AbstractObjectClass =
{
	AllClasses.get.getClassByID(ref.typ) 			
}
  /** returns the calculated result value of the term in a field
   * the field results are cached in an array. That is not pure functional and may cause problems.
   * 
   * @param index the field number
   * @return the constant result value
   */
	def fieldValue(index:Int):Constant = 	{
		if(fieldValuesCache(index)== null) {
			regenFieldCache(index)
		}
		fieldValuesCache(index)
	}
	
	def regenFieldCache(index:Int):Unit = {
		fieldValuesCache(index)= {			  
				val fieldType = getObjectClass().fields(index).typ
				val result=fieldData(index).getValue
				//println("inst "+ref+" getfield "+index+" fieldType:"+fieldType+" result:" +result)
				if(result.getType==fieldType|| result.getType==DataType.undefined )
					result // return the value
					else  // return converted value
						Constant.createConversion(result,fieldType)		
			}		  
	}	
}


object InstanceData 
{
	val transMan:ATransactionManager=null
	// reads an instance from a DataInput

	def readFields(file:DataInput)= {
		val nfields=file.readByte
		val fArray=new Array[Expression](nfields)
		for (n<- 0 until nfields)
			fArray(n)=Expression.read(file)
		fArray		
	}
	
	def readOwners(file:DataInput) = {
		val nOwners=file.readByte
		val ownArray=new Array[OwnerReference](nOwners)
		for( o<- 0 until nOwners)
			ownArray(o)= OwnerReference.read(file)
			ownArray
	}
	
	def read(nref:Reference,file:DataInput,nhasChildren:Boolean) = 		
			new InstanceData(nref,readFields(file),readOwners(file),nhasChildren)	
	
	def readWithChildInfo(nref:Reference,file:DataInput)= 
			new InstanceData(nref,readFields(file),readOwners(file),file.readBoolean)
		
		

}

/* describes one owner of an instance data
 * 
 */
case class OwnerReference(val ownerField:Byte, //in what property field of the owner instance
	//is this instance stored 
	val ownerRef:Reference) { // reference of the owner instance
	
	def write(out:DataOutput) = {
		out.writeByte(ownerField)
		ownerRef.write(out)
	}	
	
}

object OwnerReference {
	def read(in:DataInput) = new OwnerReference(in.readByte,Reference(in))	
}