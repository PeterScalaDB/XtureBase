/**
 * Author: Peter Started:04.10.2010
 */
package client.comm
import definition.data._
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
}

/*trait ReadableClass {
	def ref:Reference	
}*/