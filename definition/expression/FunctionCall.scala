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
case class FunctionCall(module:Option[String],name:String,params:List[Expression],
	var cacheValue:Constant=null) extends Expression  {
	
	def getType(): DataType.Value = { DataType.FunctionCall }  

  def getValue(): Constant = 
  { 
  	if(cacheValue==null) {
  		val paramValues:List[Constant] = for (param <-params) yield param.getValue
  		cacheValue=FunctionManager.get.getFunctionValue(module,name,paramValues)
  		cacheValue 
  	}
  	cacheValue
  }
  
  //TODO: make special read/write methods that store the cached value, for sending
  // terms via network

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
  
  
  
  override def getElementList[T <: Expression](whatType:DataType.Value,resultList:List[T]):List[T]={
  	var result=super.getElementList(whatType,resultList)
  	for(p<-params)
  		result=p.getElementList(whatType,result)
  	result
  }
  
  
  
  override def replaceExpression(checker:(Expression) => Expression): Expression =  {
  	 new FunctionCall(module,name, for(p<-params) 
  		                              yield p.replaceExpression(checker) )
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