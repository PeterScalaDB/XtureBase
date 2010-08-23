/**
 * Author: Peter Started:07.08.2010
 */
package definition.expression

import definition.expression._

/** abstract interface for a function manager
 * 
 */
trait FunctionManager {
	
	
	def getFunctionValue(module:Option[String],funcName:String,paramValues:List[Constant]):Constant
	
	//def getCollFunctionValue(funcName:String,propertyField:Byte,childType:Int,childField:Byte):Constant
	
}

object FunctionManager {
	private var theManager:FunctionManager=null
	
	def get=theManager
	def setManager(newMan:FunctionManager ) = {theManager=newMan }
}

