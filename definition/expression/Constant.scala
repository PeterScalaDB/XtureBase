/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/** base type of all Constant classes
 * 
 */
trait Constant extends Expression {  

  def getChildCount(): Int = { 0 }

  def getChildNr(ix: Int): Expression = { throw new IllegalArgumentException("Constants don't have any children, access to child nr:"+ix) }

  def isConstant(): Boolean = { true }
  
  def getValue=this  
  
  def toInt:Int
  
  def toLong:Long
  
  def toDouble:Double
  
  def toBoolean:Boolean 
  
  def convertTo(toType:DataType.Value):Constant = Constant.createConversion(this,toType)

}

object Constant
{
	/** creates a converted Contstant in the given type
	 * 
	 * @param value Original value
	 * @param toType in what type should this value be converted
	 * @return A new Constant with the value, converted to another type
	 */
	def createConversion(value:Constant,toType: DataType.Value) =
		toType match {		
		case DataType.IntTyp => new IntConstant(value.toInt)
		case DataType.LongTyp => new LongConstant(value.toLong)
		case DataType.DoubleTyp => new DoubleConstant(value.toDouble)   
		case DataType.StringTyp => new StringConstant(value.toString)
		case _ => throw new IllegalArgumentException("Conversion to type "+toType+" is not supported yet")
		}	
}

object EMPTY_EX extends Constant
{
	def getType= DataType.undefined	
	
	def createCopy:Expression= this	
	
	def toInt:Int=0
	
	def toLong:Long=0
  
  def toDouble:Double=0
  
  def toBoolean:Boolean=false 
	
	def getTerm:String ="(Empty)"
	
	def write(file:DataOutput)= { 
  	file.writeByte(DataType.undefined.id)  	
  }	
	
	override def toString = ""
	
}