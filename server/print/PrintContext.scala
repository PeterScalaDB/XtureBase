/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import definition.expression.{Constant,EMPTY_EX}
import definition.typ.{AbstractObjectClass,AllClasses}
import definition.data.InstanceData



/**
 * 
 */
trait Context {
	def getVarValue(varName:String):Constant
}

class PrintContext extends Context{
	var currentClass:AbstractObjectClass= _
	var currentInstance:InstanceData = _
	
	val printVariables=collection.mutable.HashMap[String,Constant]()
	
	def setCurrInstance(data:InstanceData)= {
		if(currentClass==null || currentClass.id!=data.ref .typ)
			currentClass=AllClasses.get.getClassByID(data.ref .typ)
		currentInstance=data
	}
	
  def getVarValue(varName:String):Constant = {
  	
  	if(printVariables.contains(varName)) printVariables(varName)
  	else {
  		if(varName.size>5&& varName.substring(0, 5).equalsIgnoreCase("field")) {
  			val fieldName=varName.substring(5)  			
  		  currentClass.fields.indices .foreach(i=>if(fieldName.equalsIgnoreCase(currentClass.fields(i).name)) {
  		  	//println("Resolve var:"+fieldName+" currClass:"+currentClass.id+" currInst:"+currentInstance.fieldValue(i))
  		  	return currentInstance.fieldValue(i)	})
  		}  		
  		EMPTY_EX
  	}
  }
}