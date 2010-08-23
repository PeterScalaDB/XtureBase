/**
 * Author: Peter Started:21.08.2010
 */
package runtime.function

import definition.expression._
import definition.data.Reference

/** super class of all collecting function descriptions
 * 
 */
abstract class CollectingFunction (val name:String) {
	
	/** returns the "null" value of that function, in the case no children are there
	 * 
	 * @return
	 */
	def emptyValue:Constant

}

abstract class SingleCollFunction(override val name:String) extends CollectingFunction(name) {
	
	/** a child was added, calculate the new result value 
	 * 
	 * @param oldResult the former result of the function
	 * @param newValue the field value of the new child
	 * @return the new result
	 */
	def childAdded(oldResult:Constant,newValue:Constant):Constant
	
	/** a field of a child was changed, calcuate the new result
	 * 
	 * @param oldResult the former result of the function
	 * @param oldValue the old value of the child field
	 * @param newValue the new value of the child field
	 * @return the new result or None if all children need to be reloaded
	 */
	def childChanged(oldResult:Constant,oldValue:Constant,newValue:Constant):Option[Constant]
	
	/** a child was removed, calculate new result
	 * 
	 * @param oldResult the former result of the functin
	 * @param oldValue 
	 * @return the new result or None if all children need to be reloaded
	 */
	def childRemoved(oldResult:Constant,oldValue:Constant):Option[Constant]
	
	
		
}


abstract class ListCollFunction(override val name:String) extends CollectingFunction(name) {
	
	/** a child was added, changed or removed, and the result list was updated. Calculate the new result
	 * 
	 * @param newList the updated list with the new values of all children
	 * @return the new result
	 */
	def listChanged(newList:List[(Reference,Constant)]):Constant
	
}