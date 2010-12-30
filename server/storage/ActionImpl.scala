/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.typ._
import definition.data._
import definition.expression._
import server.comm.UserSocket

/** Implementation of an action
 * 
 */

class ActionImpl(val name:String,override val question:Option[ParamQuestion],
	val func:(UserSocket,InstanceData,Seq[(String,Constant)]) => Boolean) extends AbstractAction  {
	def isIterator=false
	def toXML = 
   {		
  	 <Action  name={name} iter= {"0"}   >
  	 { question match {
  		 case Some(q)=>q.toXML()
  		 case _ =>  		 
  		 }
  	 }
  	 </Action>
   }
}
	
/**
 * @param name name of the iterator action
 * @param question definition of the questions for the action dialog
 * @param func the function to call with parameters(parentRef,childRefs,params)
 */
class ActionIterator(val name:String,override val question:Option[ParamQuestion],
	val func:(UserSocket,OwnerReference,Seq[InstanceData],Seq[(String,Constant)]) => Boolean) extends AbstractAction  {
	def isIterator=true	
	def toXML = 
   {		
  	 <Action  name={name} iter={"1"}  >
  	 { question match {
  		 case Some(q)=>q.toXML()
  		 case _ =>  		 
  		 }
  	 }
  	 </Action> 
   }
}


class CreateActionImpl(val name:String,override val question:Option[ParamQuestion],
	val func:(UserSocket,Seq[InstanceData],Seq[(String,Constant)],Int) => Boolean) extends AbstractAction  {
	def isIterator=false	
	def toXML = 
   {		
  	 <Action  name={name} iter={"1"}  >
  	 { question match {
  		 case Some(q)=>q.toXML()
  		 case _ =>  		 
  		 }
  	 }
  	 </Action> 
   }
}