/**
 * Author: Peter Started:07.08.2010
 */
package runtime.function

import definition.expression._
import definition.typ._
import transaction.handling._
/**
 * 
 */
object StorageFuncMan extends FunctionManager {
	
	//val p=new FunctionDescription("sin",null,null){
	//		override def getValue(param:List[Constant]):Constant = {	null} }	
	
	def checkParameters(param:List[Constant],paramDesc:List[ParDes]):Boolean =   {
		println("check params "+param+" "+paramDesc)
  	if(paramDesc.size!=param.size) return false
  	 for(i <- 0 until param.size)
  		 if(! DataType.isCompatible(param(i).getType ,paramDesc(i).typ)) return false
  	 true
  }
	
	val funcList= Map[String,FEntry] (
			("sin" -> FEntry(List(ParDes(DataType.DoubleTyp)),x => 
					new DoubleConstant(math.sin(x.head.toDouble))						
				)),
			("cos" -> FEntry(List(ParDes(DataType.DoubleTyp)),x => 
					new DoubleConstant(math.cos(x.head.toDouble))						
				)),
			("max" -> FEntry(List(ParDes(DataType.DoubleTyp),ParDes(DataType.DoubleTyp)),x => {
			    val v1=x(0).toDouble
					val v2=x(1).toDouble
					println("call max "+x)
					new DoubleConstant( if(v1<v2) v2 else v1 )	}							
				))
			)	
	
	
	def getFunctionValue(module:Option[String],funcName:String,paramValues:List[Constant]) = {
		val uname=funcName.toLowerCase()
		println("call funcman :"+funcName+" "+paramValues)
		if(funcList.contains(uname))
		{
			val entry =funcList(uname)
			if(checkParameters(paramValues,entry.params))
				entry.func(paramValues)
			else EMPTY_EX 
			//TODO notify when wrong function parameters passed
		}		  
		else throw new IllegalArgumentException("Function "+uname+" not found")		
	}
	
	val collFuncList= Map[String,CollectingFunction] (
	    ("doubleSum" -> new SingleCollFunction("doubleSum"){
	    	def childAdded(oldResult:Constant,newValue:Constant):Constant = {
	    	  new DoubleConstant(oldResult.toDouble+newValue.toDouble)	
	    	}	
	      def childChanged(oldResult:Constant,oldValue:Constant,newValue:Constant):Option[Constant]	= {
	      	Some(new DoubleConstant(oldResult.toDouble-oldValue.toDouble+newValue.toDouble))
	       }
		    def childRemoved(oldResult:Constant,oldValue:Constant):Option[Constant] = {
		    	Some(new DoubleConstant(oldResult.toDouble-oldValue.toDouble))
		    }		   
	    	def emptyValue:Constant = new DoubleConstant(0)	
	    })		
	    
	)
	
	
	
	
}


// *********************************** HELPER CLASSES ********************************************



case class FEntry(params:List[ParDes],func:(List[Constant])=>Constant)

// parameter description
case class ParDes(typ:DataType.Value,name:String="",help:String=""  )