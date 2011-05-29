/**
 * Author: Peter Started:04.10.2010
 */
package client.comm
import definition.data._
import definition.expression.Expression
import java.io._

/** a factory that creates instances of user defined classes
 * 
 */
abstract class SubscriptionFactory [T <: Referencable] {
	
	val typeMap=collection.mutable.HashMap[Int,(Reference,DataInput)=>T]()
	def emptyFunc(ref:Reference):T
	
  def registerClass(typID:Int,createFunc: (Reference,DataInput)=>T)	= typeMap(typID)=createFunc
  
  def createObject(nref:Reference,in:DataInput):T = typeMap(nref.typ)(nref,in)
  def createEmptyObject(nref:Reference):T = emptyFunc(nref)
  
  
  
  def loadFields (in:DataInput,numFields:Byte):(Array[OwnerReference],Array[OwnerReference],Seq[Expression]) = {
		val nfields=in.readByte
		if(nfields!=numFields) throw new IllegalArgumentException("wrong number of fields "+nfields+" ")
		val retList=for(i <- 0 until nfields) yield Expression.read(in)		
		val owners=InstanceData.readOwners(in)
		val suOwners=InstanceData.readSecondUseOwners(in)		
		in.readBoolean	
		(owners,suOwners,retList)
	}
}

/*trait ReadableClass {
	def ref:Reference	
}*/