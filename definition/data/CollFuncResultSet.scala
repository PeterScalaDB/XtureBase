/**
 * Author: Peter Started:12.08.2010
 */
package definition.data

import java.io._
import definition.expression._
import runtime.function._
import transaction.handling._
import definition.typ._

/** a Flag to signal that a field in a parent instance has a collectingFunction
 * 
 */
abstract class CollFuncResult(val funcName:String,val childType:Int,val childField:Byte,val parentField:Byte,
	val parentPropField:Byte) {
	
	def write(file:DataOutput) = 	{
		file.writeUTF(funcName)
		file.writeInt(childType)
		file.writeByte(childField)
		file.writeByte(parentField)
		file.writeByte(parentPropField)			
	}  
	
	/** checks if this result fits to the given function call
	 * 
	 * @param call the function call to test
	 * @param pfieldNr the field nr where  the call is located
	 * @return true if they fit
	 */
	def fitsToFuncCall(call:CollectingFuncCall,pfieldNr:Byte):Boolean = {
		pfieldNr==parentField && call.name == funcName && call.childType==childType && call.childField==childField && 
		call.propertyField==parentPropField
	}
}

class SingleCollFuncResult(override val funcName:String,override val childType:Int,override val childField:Byte,override val parentField:Byte,
	override val parentPropField:Byte,val resultValue:Constant) extends 
	CollFuncResult(funcName,childType,childField,parentField,parentPropField)
{
	override def write(file:DataOutput) = {
		file.writeBoolean(false)
		super.write(file)
		resultValue.write(file)
	}
	override def toString=" SingleResult "+funcName+" childType:"+childType+ " childField:"+childField+" parField:"+parentField+" parPropField:"+
	     parentPropField+" result:"+resultValue
}

class ListCollFuncResult(override val funcName:String,override val childType:Int,override val childField:Byte,override val parentField:Byte,
	override val parentPropField:Byte,val resultList:List[(Reference,Constant)]) extends 
	CollFuncResult(funcName,childType,childField,parentField,parentPropField)
{
	override def write(file:DataOutput) = {
		file.writeBoolean(true)
		super.write(file)
		file.writeInt(resultList.length)
		for(r <-resultList)
		{
			r._1.write(file)
			r._2.write(file)
		}		
	}
	override def toString=" ListResult "+funcName+" childType:"+childType+ " childField:"+childField+" parField:"+parentField+" parPropField:"+
	     parentPropField+" resultList:"+resultList
}

//*********************************************************************************************************************

/** a list of collectingFuncFlags stored for a parent instance
 * 
 */
class CollFuncResultSet(override val ref:Reference,
	val callResultList:List[CollFuncResult]) 
      extends Referencable
{
	 override def write(file:DataOutput) = 
	 {
		 file.writeInt(callResultList.size)
		 for(f <-callResultList)
			 f.write(file)
	 }
	 
	 /** creates a copy of the ResultSet with the given Reference
	  * 
	  * @param newRef the new Reference
	  * @return a copy of the ResultSet
	  */
	  def changeReference(newRef:Reference)= new CollFuncResultSet(newRef,callResultList)
	 
	 /** removes the given Function Call from the List and returns a new version of this set
	  * 
	  * @param old the function call to remove
	  * @return a new version of the set with the call removed
	  */
	 def removeCollCall(old:CollectingFuncCall ,fieldNr:Byte):CollFuncResultSet =	 {		 
		 new CollFuncResultSet(ref, callResultList.filter(c => ! c.fitsToFuncCall(old,fieldNr))) 
	 }
	 
	 /** adds the given Function Call to the set, and calculates the initial value
	  * 
	  * @param newCall the new function call to add
	  * @param pFieldNr in what field are the new calls added
	  * @return tuple: (a new version of the set containing the new call, the new result value)
	  */
	 def addCollCall(newCall :CollectingFuncCall,pFieldNr:Byte) : (CollFuncResultSet,Constant) = {
		 if (!StorageFuncMan.collFuncList.contains(newCall.name)) throw new IllegalArgumentException("CollFunction "+newCall.name+ " not defined !")
		 val func = StorageFuncMan.collFuncList(newCall.name)
		  func match {
			 case s: SingleCollFunction => {
				 val result= new SingleCollFuncResult(newCall.name,newCall.childType,newCall.childField,pFieldNr,
					  newCall.propertyField,calculateSingleResult(newCall.propertyField,newCall.childType,newCall.childField,s))
				 (new CollFuncResultSet(ref,result :: callResultList),result.resultValue)
			 }
	        
			 case l: ListCollFunction =>{
				 val result= new ListCollFuncResult(newCall.name,newCall.childType,newCall.childField,pFieldNr,
					  newCall.propertyField,calculateListResult(newCall))
				 (new CollFuncResultSet(ref,result :: callResultList),l.listChanged(result.resultList))
			 }	       
		 }			  
	 }	
	 
	 
	 
	 /** calculates the result Value of that CollFunk by loading all child instances and using their values
	 * 
	 * @param call the function call to calculate
	 * @param func the function
	 * @return the result of collecting all data from the children
	 */
	def calculateSingleResult(propField:Byte,cType:Int,cField:Byte,func:SingleCollFunction):Constant = 	{
		System.out.println("calculate SingleResult")
		ActionList.getInstanceProperties(ref) match {
			case Some(prop) => { // if there are children
				var result=func.emptyValue // init loop variable
				for(cRef <- prop.propertyFields(propField ).propertyList ) // run through all children
					{print(" checking "+cRef)
					if ( AllClasses.get.getClassByID(cRef.typ).inheritsFrom(cType) ) // when they fit
					{ 
						val instData=ActionList.getInstanceData(cRef)
						System.out.println(" fits "+instData)
						if(instData!=null) // null == instance should be deleted
						result=func.childAdded(result,instData.fieldValue(cField ))
					}}
				result
			}				                                  
			case _ => func.emptyValue // if there arent any children
		}		
	}
	
	/** calculates the result Value of that CollFunk by loading all child instances and using their values
	 * 
	 * @param call the function call to calculate
	 	 * @return the list of all child values
	 */
	def calculateListResult(call:CollectingFuncCall):List[(Reference,Constant)] = {
		ActionList.getInstanceProperties(ref) match {
			case Some(prop) => { // if there are children
				//val result:List[(Reference,Constant)]= 
					(for(cRef <- prop.propertyFields(call.propertyField ).propertyList ; // run through all children
						if ( AllClasses.get.getClassByID(cRef.typ).inheritsFrom(call.childType) );
					  //TODO: check the class Version to find out inheritance of children !
						val instData=ActionList.getInstanceData(cRef);if instData!=null )// when they fit
					yield (ref,instData.fieldValue(call.childField )) ).toList					
				//result
			}				                                  
			case _ => Nil // if there arent any children
		}		
	}
	
	
	/** notifies the funcResult that a child has changed
	 * 
	 * @param fRes the FuncResult element that collects the child info
	 * @param oldValue the old value of the child field
	 * @param newValue the new Value of the child field
	 * @return a tuple: (the new State of the FuncResult,the new result)
	 */
	def childChanged(fRes:CollFuncResult,childRef:Reference,oldValue:Constant,newValue:Constant):(CollFuncResult,Constant) = {
		val func = StorageFuncMan.collFuncList(fRes.funcName)
		
		  func match {
			 case s: SingleCollFunction => {
				  val newResultValue= s.childChanged(fRes.asInstanceOf[SingleCollFuncResult].resultValue , 
				  	oldValue, newValue) match {
				  		case Some(a) => a // the function could calculate a value
				  		case None => // all instances need to be recalculated
				  	     calculateSingleResult(fRes.parentPropField,fRes.childType,fRes.childField,s)
				    }
				  (new SingleCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
					  fRes.parentPropField,newResultValue),newResultValue)
			 }
			 case l: ListCollFunction => {
				  val rList:List[(Reference,Constant)]= for (item <-fRes.asInstanceOf[ListCollFuncResult].resultList) 
				  	yield if (item._1==childRef) (childRef,newValue) else item 
				  
				 ( new ListCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
					  fRes.parentPropField,rList),l.listChanged(rList))
			 }
		}	 
	}
	
	
	def childDeleted(fRes:CollFuncResult,childRef:Reference,oldValue:Constant):(CollFuncResult,Constant) = {
		val func = StorageFuncMan.collFuncList(fRes.funcName)
		func match {
			 case s: SingleCollFunction => {
				  val newResultValue= s.childRemoved(fRes.asInstanceOf[SingleCollFuncResult].resultValue , 
				  	oldValue) match {
				  		case Some(a) => a // the function could calculate a value
				  		case None => // all instances need to be recalculated
				  	     calculateSingleResult(fRes.parentPropField,fRes.childType,fRes.childField,s)
				    }
				  (new SingleCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
					  fRes.parentPropField,newResultValue),newResultValue)
			 }
			 case l: ListCollFunction => {
				  val rList:List[(Reference,Constant)]= fRes.asInstanceOf[ListCollFuncResult].resultList.filter(item => item._1 != childRef)
				  
				 ( new ListCollFuncResult(fRes.funcName,fRes.childType,fRes.childField,fRes.parentField,
					  fRes.parentPropField,rList),l.listChanged(rList))
			 }
		}
	}	
	
	override def toString="(CollSet " + ref + ":"+(callResultList.mkString(", ")+") ")
	
}

object CollFuncResultSet
{
	private def readSingleResult(file:DataInput) =
	{
		new SingleCollFuncResult(file.readUTF,file.readInt,file.readByte,file.readByte,file.readByte,Expression.read(file).asInstanceOf[Constant])
	}
	
	
	
	private def readListResult(file:DataInput) = 
	{
		new ListCollFuncResult(file.readUTF,file.readInt,file.readByte,file.readByte,file.readByte,
			{
        val length=file.readInt
        (for(i <-0 until length) yield (Reference(file),Expression.read(file).asInstanceOf[Constant])).toList
			})
	}
	
	private def readResult(file:DataInput) = 
	{
		if(file.readBoolean) readListResult(file)
		else readSingleResult(file)
	}
	
	
	
	
	
	def read(nref:Reference,file:DataInput) = {
		val size=file.readInt()
		val newList:List[CollFuncResult]=
			 (for (i <- 0 until size) 
			          yield readResult(file)).toList 
		new CollFuncResultSet(nref,newList)
	}
	
	
}
