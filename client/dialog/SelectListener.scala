/**
 * Author: Peter Started:09.10.2010
 */
package client.dialog

import definition.data._
import definition.typ.SelectGroup


/** a  component that sends out selection messages 
 * 
 */

trait SelectSender {
	def deselect(notify:Boolean)
	def registerSelectListener(listener:SelectListener)
}


/** a component that receives select messages
 * 
 */
trait SelectListener {
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]])
}



object EMPTY_GROUP extends SelectGroup[Referencable](null,Seq.empty) {
	val list=List(this)
}

