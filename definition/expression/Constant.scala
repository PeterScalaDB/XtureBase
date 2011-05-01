/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}
import java.util.Date

/** base type of all Constant classes
 * 
 */
trait Constant extends Expression {  

  def getChildCount(): Int = { 0 }

  def getChildNr(ix: Int): Expression = { println("Constants don't have any children, access to child nr:"+ix);this }

  def isConstant(): Boolean = { true }
  
  def getValue=this  
  
  def toInt:Int
  
  def toLong:Long
  
  def toDouble:Double
  
  def toFloat=toDouble.toFloat
  
  def toCurrency= new CurrencyConstant(Math.round(toDouble*100))
  
  def toBoolean:Boolean 
  
  def toVector=new VectorConstant(0,0,0)
  
  def convertTo(toType:DataType.Value):Constant = Constant.createConversion(this,toType)
  
  def getNative:Any
  
  def toDate=new Date(toLong)
  
  def isNumberConstant=false  
}

object Constant
{
	/** creates a converted Contstant in the given type
	 * 
	 * @param value Original value
	 * @param toType in what type should this value be converted
	 * @return A new Constant with the value, converted to another type
	 */
	def createConversion(value:Constant,toType: DataType.Value):Constant = {
		if(value.getType==toType) return value
		val res=toType match {		
		case DataType.IntTyp|DataType.EnumTyp => new IntConstant(value.toInt)
		case DataType.LongTyp => new LongConstant(value.toLong)
		case DataType.DoubleTyp => new DoubleConstant(value.toDouble)   
		case DataType.StringTyp => new StringConstant(value.toString)
		case DataType.BoolTyp => new BoolConstant(value.toBoolean)
		case DataType.CurrencyTyp => value.toCurrency
		case DataType.VectorTyp =>val nv=value.toVector;new VectorConstant(nv.x,nv.y,nv.z)
		case DataType.DateTyp => new DateConstant(new Date(value.toLong))
		case DataType.BlobTyp =>Expression.NullBLOB 
		case _ => throw new IllegalArgumentException("Conversion of "+value+"  to type "+toType+" is not supported yet")
		}
		//System.out.println("create conversion "+value+":"+value.getType+" toType:"+toType+" "+res)
		res
	}
	
	def getNativeNull(typ:DataType.Value)= typ match {
	  case DataType.IntTyp|DataType.EnumTyp  => 0
		case DataType.LongTyp => 0L
		case DataType.DoubleTyp => 0d   
		case DataType.StringTyp => ""
		case DataType.BoolTyp => false
		case DataType.VectorTyp =>NULLVECTOR
		case DataType.CurrencyTyp =>ImBroke
		case DataType.BlobTyp => Array[Byte]()
		case DataType.DateTyp => DateConstant.nativeNull
		case _=> ""
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
  
  override def toCurrency=ImBroke
	
	def getTerm:String =""
	
	def write(file:DataOutput)= { 
  	file.writeByte(DataType.undefined.id)  	
  }
	
	def getNative=""
	
	override def toString = ""
		
  override def isNullConstant=true
  
  override def toDate=DateConstant.nativeNull
	
}