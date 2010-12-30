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
class InstanceData (override val ref:Reference,	val fieldData:IndexedSeq[Expression],	 									
	val owners:Array[OwnerReference]=Array(),val secondUseOwners:Seq[OwnerReference]=Seq.empty,val hasChildren:Boolean) extends Referencable
	{	
	lazy val fieldValue:Seq[Constant]=regenFieldCache
	
	lazy val theClass=getObjectClass()
	lazy val shortFormArray=getFormatArray(theClass.shortFormat.fields)
	lazy val resultFormArray=getFormatArray(theClass.resultFormat.fields)
	
	override def toString() = {
		if(theClass.shortFormat!=NOFORMAT&&fieldData.length!=0)
			try {
			theClass.shortFormat.formStr.format(shortFormArray:_*)
			}
			catch {
				case e:Exception=> e.toString
			}
		else theClass.name+" "+ref.sToString		
	}
	
	/** creates an Array of the current values of the fields that make up an format
	 * 
	 * @param fieldIndexes array of the field numbers that are part of the format
	 * @return an array of the current native values of the given fields
	 */
	def getFormatArray(fieldIndexes:Array[Int]):Array[Any] = {
		for(i<-fieldIndexes) 
			yield if(i> -1){
				val theVal=fieldValue(i)
				 theVal.getNative 
			}
			else ref.instance
	}
	
	def resultString= {
		if(theClass.resultFormat!=NOFORMAT) {
			//System.out.println("resultArray:"+resultFormArray.mkString)
			//System.out.println(fieldValue(0).getType)
			try {
			theClass.resultFormat.formStr.format(resultFormArray:_*)
			}
			catch {
				case e:Exception=> e.toString
			}
			//"RESULT"
		}
			
		else ""
	}


	override def write(file:DataOutput) = 	{
		//ref.write(file)

		file.writeByte(fieldData.length)
		for(field<-fieldData)
		{			
			field.write(file)
		}
		// owners
		file.writeByte(owners.length)
		for(owner<-owners)
			owner.write(file)
			// secondUseOwners
		file.writeShort(secondUseOwners.length)
		for(so<-secondUseOwners)
			so.write(file)
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
		val newArray:IndexedSeq[Expression]=(for (i <- 0 until fieldData.length) 
			yield (if (i==fieldNr) newValue else fieldData(i)))
			new InstanceData(ref,newArray,owners,secondUseOwners,hasChildren)
	}

	def changeOwner(newOwners:Array[OwnerReference]) = {
		new InstanceData(ref,fieldData,newOwners,secondUseOwners,hasChildren)
	}
	
	def changeSecondUseOwners(newOwners:Seq[OwnerReference]) = {
		new InstanceData(ref,fieldData,owners,newOwners,hasChildren)
	}
	
	def addSecondUseOwner(newOwner:OwnerReference)= {
		changeSecondUseOwners(secondUseOwners:+ newOwner)
	}
	
	

	def setHasChildren(newValue:Boolean) = {
		new InstanceData(ref,fieldData,owners,secondUseOwners,newValue)
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
	def clone(newRef:Reference,newOwners:Array[OwnerReference],newSecondUseOwners:Seq[OwnerReference]):InstanceData =	{
		new InstanceData(newRef,fieldData.map(_.createCopy),newOwners,newSecondUseOwners,hasChildren)
	}

	/** replaces an ownerReferene with another ref and returns a new Instance with the new values
	 * 
	 * @param fromRef the old ref to remove
	 * @param toRef the new ref that replaces the old one
	 */
	def changeSingleOwner(fromRef:OwnerReference,toRef:OwnerReference):InstanceData = {
		val newOwnerList= for (ref <- owners)
			yield (if (ref==fromRef) toRef else ref)
			new InstanceData(ref,fieldData,newOwnerList,secondUseOwners,hasChildren)
	}


	def getObjectClass():AbstractObjectClass =
	{
		AllClasses.get.getClassByID(ref.typ) 			
	}
	
	/*def fieldValue(index:Int):Constant = 	{
		if(fieldValuesCache(index)== null) {
			regenFieldCache(index)
		}
		fieldValuesCache(index)
	}*/

	def regenFieldCache:Seq[Constant] = for(index<-0 until fieldData.size)yield {					  
			val fieldType = theClass.fields(index).typ
			val result=fieldData(index).getValue
			//System.out.println("inst "+ref+" getfield "+index+" fieldType:"+fieldType+" result:" +result)
			if(result==null) { 
				System.out.println("result== null "+ref+" field:"+index+" "+fieldData(index))
				EMPTY_EX
			} else			 			
			if(result.getType==fieldType )
				result // return the value
				else  // return converted value
					Constant.createConversion(result,fieldType)						  
	}	
}


object InstanceData 
{
	val transMan:ATransactionManager=null
	// reads an instance from a DataInput

	def readFields(file:DataInput):IndexedSeq[Expression]= {
		val nfields=file.readByte
		for(i <-0 until nfields) yield Expression.read(file)
		/*val fArray=new Array[Expression](nfields)
		for (n<- 0 until nfields)
			fArray(n)=Expression.read(file)
			fArray	*/	
	}

def readOwners(file:DataInput) = {
	val nOwners=file.readByte
	val ownArray=new Array[OwnerReference](nOwners)
	for( o<- 0 until nOwners)
		ownArray(o)= OwnerReference.read(file)
		ownArray
}

def readSecondUseOwners(file:DataInput) = {
	val nOwners=file.readShort
	val ownArray=new Array[OwnerReference](nOwners)
	for( o<- 0 until nOwners)
		ownArray(o)= OwnerReference.read(file)
		ownArray
}


def read(nref:Reference,file:DataInput,nhasChildren:Boolean) = 
	/*if(nref.isNull) new InstanceData(nref,Array(),Array(),false)
	else*/ new InstanceData(nref,readFields(file),readOwners(file),readSecondUseOwners(file),nhasChildren)	

def readWithChildInfo(nref:Reference,file:DataInput)= 
	new InstanceData(nref,readFields(file),readOwners(file),readSecondUseOwners(file),file.readBoolean)



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
	override def toString= ((if(ownerRef==null)"()" else ownerRef.sToString)+"|"+ownerField)
}
object OwnerReference {
	def read(in:DataInput) = new OwnerReference(in.readByte,Reference(in))	
}


