/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import definition.expression.{Constant,EMPTY_EX}
import definition.typ.{AbstractObjectClass,AllClasses}
import definition.data.InstanceData
import definition.expression.IntConstant
import definition.expression.DateConstant
import definition.data.PlaceHolderElement


case class PlaceHolderValue(var holderList:List[PlaceHolderElement],var value:Option[String]=None)

/**
 * 
 */
trait Context {
	def getVarValue(varName:String):Constant
	def createPlaceHolder(name:String,el:PlaceHolderElement):Option[String]
	def setPlaceHolderValue(name:String,newValue:String)
}

class PrintContext extends Context{
	var currentClass:AbstractObjectClass= _
	var currentInstance:InstanceData = _
	
	val placeHolders=collection.mutable.Map[String,PlaceHolderValue]()
	
	
	val printVariables=collection.mutable.HashMap[String,Constant]()
	
	def initPrintSession()= {
		placeHolders.clear
	}
	
	def setCurrInstance(data:InstanceData)= {
		if(currentClass==null || currentClass.id!=data.ref .typ)
			currentClass=AllClasses.get.getClassByID(data.ref .typ)
		currentInstance=data
	}
	
	def setPageNr(value:Int )={
		printVariables("pageNr")=new IntConstant(value)
	}
	
	def setPrintDate(value:DateConstant) = {
		printVariables("date")=value
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
  
  def createPlaceHolder(name:String,el:PlaceHolderElement):Option[String] = {
		if(placeHolders.contains(name)){
			val vel=placeHolders(name)			
			vel.holderList=el :: vel.holderList
			if(vel.value .isDefined)el.value=vel.value.get
			vel.value
		}
		else {
			placeHolders(name)=new PlaceHolderValue( List(el))
			None
		}
  }
  
	def setPlaceHolderValue(name:String,newValue:String)= {
		println("set placeHolder "+name+" '"+newValue+"'")
		if(placeHolders.contains(name)){
			val vel=placeHolders(name)
			vel.holderList.foreach(_.value =newValue)
			vel.value =Some(newValue)
		}
		else println("set placeholder, unknown name:"+name+" ->" +newValue)
	}
}