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
object CommonFuncMan extends FunctionManager {
	
	//val p=new FunctionDescription("sin",null,null){
	//		override def getValue(param:List[Constant]):Constant = {	null} }	
	
	def checkParameters(param:List[Constant],paramDesc:List[ParDes]):String =   {
		//System.out.println("check params "+param+" "+paramDesc)
  	if(paramDesc.size!=param.size) return "Wrong numbers of parameters "+param.size+", expected:"+paramDesc.size
  	 for(i <- 0 until param.size)
  		 if((paramDesc(i).typ!=DataType.undefined)&&(! DataType.isCompatible(param(i).getType ,paramDesc(i).typ))) 
  			 return ("Wrong "+i+". function parameter type "+param(i).getType+", expected Type:"+paramDesc(i)  )
  	 null
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
				System.out.println("call max "+x)
				new DoubleConstant( if(v1<v2) v2 else v1 )	}							
			)),
			("if" -> FEntry(List(ParDes(DataType.BoolTyp),ParDes(DataType.undefined),ParDes(DataType.undefined)),x=> {
				if(x(0).toBoolean) x(1).getValue
				else x(2).getValue
			})),
			("v" -> FEntry(List(ParDes(DataType.DoubleTyp),ParDes(DataType.DoubleTyp),ParDes(DataType.DoubleTyp)),x=> {
				new VectorConstant(x(0).toDouble,x(1).toDouble,x(2).toDouble)
			}))
	)	
	
	
	def getFunctionValue(module:Option[String],funcName:String,paramValues:List[Constant]) = {
		val uname=funcName.toLowerCase()
		//System.out.println("call funcman :"+funcName+" "+paramValues)
		if(funcList.contains(uname))
		{
			val entry =funcList(uname)
			val error=checkParameters(paramValues,entry.params)
			if(error!=null)throw new IllegalArgumentException(error+" in function "+funcName+"\n params:"+
				paramValues.mkString(",")+"\n types:" +paramValues.map(_.getClass.toString).mkString(","))
			entry.func(paramValues)			
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