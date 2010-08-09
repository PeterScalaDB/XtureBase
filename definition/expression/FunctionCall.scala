/**
 * Author: Peter Started:07.08.2010
 */
package definition.expression

import definition.typ._
import definition.expression._
import java.io._


/** An expression that manages a function call
 * 
 */
case class FunctionCall(module:Option[String],name:String,params:List[Expression]) extends Expression  {
	def getType(): DataType.Value = { DataType.FunctionCall }
  
  var cacheValue:Constant=null

  def getValue(): Constant = 
  { 
  	if(cacheValue==null) {
  		val paramValues:List[Constant] = for (param <-params) yield param.getValue
  		cacheValue=FunctionManager.get.getFunctionValue(module,name,paramValues)
  		cacheValue 
  	}
  	cacheValue
  }
  
  

  def createCopy(): Expression = { new FunctionCall(module,name,params) }

  def getChildCount(): Int = { params.size }

  def getChildNr(ix: Int): Expression = { params(ix)}
  
  def getTerm(): String = { (module match {
  	case Some(mod)=> mod +"."
  	case _ => ""})+name+ "( "+ params.map(_.getTerm).mkString("; ")+" ) " }

  def isConstant(): Boolean = { false }
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.FunctionCall.id)
  	file.writeBoolean(module!=None )
  	if(module!=None) file.writeUTF(module.get)
  	file.writeUTF(name)
  	file.writeByte(params.size)
  	for(p<-params)
  		p.write(file)  	
  }
  
  // looks for fieldreferences in both operands
  override def getFieldReferences (resultList:List[FieldReference]) = {
  	var result=resultList
  	for(p<-params)
  		result=p.getFieldReferences(result)
  	result
  }
  
  override def replaceFieldRefWithValue(checker:(FieldReference)=> Boolean):Expression = {
  	new FunctionCall(module,name, for(p<-params) 
  		                              yield p.replaceFieldRefWithValue(checker) )  	
  }
}

object FunctionCall {
	def apply (file: DataInput):Expression = {
		val hasModule=file.readBoolean
		var moduleName:String=""
		if(hasModule) moduleName=file.readUTF
		new FunctionCall(if(hasModule) Some(moduleName) else None,file.readUTF,
			(for(i <- 0 until file.readByte) yield Expression.read(file)).toList)
	}
}