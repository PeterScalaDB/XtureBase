/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.typ._
import definition.data._
import definition.expression._

/** Implementation of an action
 * 
 */

class ActionImpl(val name:String,override val question:Option[ParamQuestion],
	val func:(Seq[InstanceData],Seq[Constant]) => Boolean) extends AbstractAction  {
	
	def toXML = 
   { 
		println("Write Action "+name+ "  "+question)
  	 <Action  name={name} >
  	 { question match {
  		 case Some(q)=>q.toXML()
  		 case _ =>  		 
  		 }
  	 }
  	 </Action> 
   } 
	

}