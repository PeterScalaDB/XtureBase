/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import definition.expression.{Expression,Constant,EMPTY_EX}
import definition.typ.DataType
import java.io.DataOutput

/**
 * 
 */
class PrintVariable(val name:String) extends Expression {
	println("New PrintVariable:"+name)
	var value:Constant=EMPTY_EX
	
	def getType:DataType.Value=value.getType
	
	def getValue:Constant=value
	
	def createCopy:Expression=new PrintVariable(name)
	
	def getChildCount:Int=0
	
	def getChildNr(ix:Int):Expression= null
	
	def getTerm:String="PR_"+name
	
	def isConstant:Boolean	=false
	
	def write(file:DataOutput):Unit = {
     file.writeByte(DataType.StringTyp.id)
     file.writeUTF("PR_"+name)
	}
	
	override def toString=getTerm
	
	def updateVariable(cx:Context)= {
		value=cx.getVarValue(name)
	}

}

class PlaceHolderPrintVariable(nname:String) extends PrintVariable(nname)