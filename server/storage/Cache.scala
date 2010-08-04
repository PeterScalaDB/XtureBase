/**
 * Author: Peter Started:27.07.2010
 */
package server.storage

import definition.data._
import server.storage._
import definition.typ.ObjectClass
import scala.collection.mutable.ArraySeq
import scala.reflect.Manifest


/** caches the Instances of a Type
 * 
 */
class Cache[T <: Referencable](val typ:Int)(implicit m: Manifest[T]) {
	final val cacheSize=30
  //val cache:Array[T]= Array(cacheSize)	
	val cache=new ArraySeq[T](cacheSize)
	
  var pointer=0
  var cacheHit=0
  var cacheMiss=0
  var added=0
	
  def getInstanceData (inst:Long):Option[T] = {
  	for(i <- 0 until cacheSize)
  		if(cache(i)!=null && cache(i).ref.instance ==inst ) {
  			cacheHit +=1
  			return Some(cache(i))
  		}
  	cacheMiss +=1
  	None
  }
	
	def putInstanceData(inst:T):Unit = {
		added+=1
		for(i <- 0 until cacheSize)
  		if(cache(i)!=null && cache(i).ref.instance ==inst.ref .instance  ) {
  			cache(i)=inst
  			return 
  		}
		pointer += 1
		if(pointer >=cacheSize) pointer = 0
		cache(pointer)=inst
	}	
	
	override def toString() = m.toString+ "-Cache for type "+typ+" miss:"+cacheMiss+" hit:"+cacheHit+ " add:"+added+" P:"+pointer
	
}


