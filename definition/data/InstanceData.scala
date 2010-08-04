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
class InstanceData (override val ref:Reference,val classVersion:Byte,
	val fieldData:Array[Expression],	 									
	val owners:Array[OwnerReference]=Array()) extends Referencable
	{
	
	private var theClassVersion:ClassVersion=null
	private var fieldValuesCache:Array[Constant]=new Array(fieldData.length)


override def toString() = "inst" + ref.sToString+ "("+ fieldData.mkString(", ")+ ")"


override def write(file:DataOutput) = 	{
	//ref.write(file)
	file.writeByte(classVersion)
	file.writeByte(fieldData.length)
	for(field<-fieldData)
	{			
		field.write(file)
	}

	file.writeByte(owners.length)
	for(owner<-owners)
	{
		file.writeByte(owner.ownerField)
		owner.ownerRef.write(file)
	}
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
		new InstanceData(ref,classVersion,newArray,owners)
}

def changeOwner(newOwners:Array[OwnerReference]) = {
	new InstanceData(ref,classVersion,fieldData,newOwners)
}

/** creates a copy of this instance
 * a helper routine for copying instances.
 * At the moment it creates a clone with the same class version.
 * There could be another version converting an instance from an old version to a new one,
 * having a translation list for fileds.
 * 
 * @param newRef the reference of the clone instance
 * @param newOwners the owners of the new instance
 * @return
 */
def clone(newRef:Reference,newOwners:Array[OwnerReference]):InstanceData =	{
	new InstanceData(newRef,classVersion,fieldData,newOwners)
}


private def getClassVersion:ClassVersion =
{
		if(theClassVersion==null) {
			theClassVersion = ( AllClasses.getClassByID(ref.typ).getVersion(classVersion) match
					{
						case Some(a) => a
						case None => throw new IllegalArgumentException("Classversion "+classVersion+" not found in "+ref)
					})
		}
		theClassVersion
}

	def fieldValue(index:Int):Constant = 
	{
		if(fieldValuesCache(index)== null) {
			fieldValuesCache(index)= {
				val fieldType = getClassVersion.field(index).typ
				val result=fieldData(index).getValue
				//println("inst "+ref+" getfield "+index+" fieldType:"+fieldType+" result:" +result)
				if(result.getType==fieldType|| result.getType==DataType.undefined )
					result // return the value
					else  // return converted value
						Constant.createConversion(result,fieldType)		
			}
		}
		fieldValuesCache(index) 

	}
	}




object InstanceData 
{
	val transMan:ATransactionManager=null
	// reads an instance from a DataInput

	def read(nref:Reference,file:DataInput) = 	{		
	val version=file.readByte
	val nfields=file.readByte
	val fArray=new Array[Expression](nfields)		
	for (n<- 0 until nfields)
		fArray(n)=Expression.read(file)
		val nOwners=file.readByte
		val ownArray=new Array[OwnerReference](nOwners)
		for( o<- 0 until nOwners)
			ownArray(o)=new OwnerReference(file.readByte,Reference(file))
	new InstanceData(nref,version,fArray,ownArray)
}		

}

/* describes one owner of an instance data
 * 
 */
case class OwnerReference(val ownerField:Byte, //in what property field of the owner instance
	//is this instance stored 
	val ownerRef:Reference) // reference of the owner instance