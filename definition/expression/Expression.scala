/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import java.io.{DataInput,DataOutput}
import definition.typ.DataType
import java.util.Date
/** base type for all expression classes
 * 
 */
trait Expression 
{
	def getType:DataType.Value
	
	def getValue:Constant
	
	def createCopy:Expression
	
	def getChildCount:Int
	
	def getChildNr(ix:Int):Expression
	
	def getTerm:String
	
	def isConstant:Boolean	
	
	def write(file:DataOutput):Unit
	
	def isNullConstant:Boolean=false
	
	/** is called when generating an instance 
	 * will be overridden by generatorConstants like $Now and $Today
	 * 
	 * @return the actual state of that expression during generation time
	 */
	def generate:Expression =createCopy
	
	
	/** returns all Elements of the given type from this expression
	 * 
	 * @param whatType Type-Id from definition.typ.DataType
	 * @param resultList a List of elements of the given type. The elements of this expression will be added to that list.
	 * @return a new list containing all elements of resultList and the elements of this list fitting to the given type
	 */
	def getElementList[T <: Expression](whatType:DataType.Value,resultList:List[T]):List[T]={
		if(this.getType==whatType) this.asInstanceOf[T]::resultList
		else resultList
	}		
	
	/** replaces certain elements of this term
	 * 
	 * @param checker a function that is given a certain part of this term and it gives a replacement
	 * @return the new term with all replacements
	 */
	def replaceExpression(checker:(Expression) => Expression): Expression = 
		checker(this)
		
	
		def foreach(f:(Expression)=>Unit):Unit = {
		f(this)
		for(i<-0 until getChildCount) {
			//println("Foreach sub:"+getChildNr(i))
			getChildNr(i).foreach(f)
		}
	}
	
	
}

trait NullConstant extends Expression  {
		override def isNullConstant=true
		override def write(file:DataOutput)= { 
  	file.writeByte(DataType.undefined.id)  	
  }
	}

object Expression
{
	def read(file:DataInput):Expression =
	{
		val code=file.readByte
		if(code<0 || code>DataType.maxId) throw new IllegalArgumentException("Reading Expression: cant find Datatype for code: "+code)
		DataType(code) match {
			case DataType.IntTyp => new IntConstant(file.readInt)
			case DataType.LongTyp => new LongConstant(file.readLong)
			case DataType.DoubleTyp => new DoubleConstant(file.readDouble)
			case DataType.StringTyp => new StringConstant(file.readUTF)			
			case DataType.BinOp => BinaryOperation(file)
			case DataType.FieldRefTyp => FieldReference(file)
			case DataType.FunctionCall => FunctionCall(file)
			case DataType.CollFunctionCall => CollectingFuncCall(file)
			case DataType.BoolTyp => new BoolConstant(file.readBoolean)
			case DataType.VectorTyp => new VectorConstant(file.readDouble,file.readDouble,file.readDouble)
			case DataType.CurrencyTyp => new CurrencyConstant(file.readLong)
			case DataType.ParentRefTyp => ParentFieldRef(file)
			case DataType.BlobTyp =>BlobConstant(file) 
			case DataType.DateTyp =>DateConstant(file)
			case _ => EMPTY_EX
		}		
	}
	
	def readConstant(file:DataInput):Constant =
	{
		DataType(file.readByte()) match {
			case DataType.IntTyp => new IntConstant(file.readInt)
			case DataType.LongTyp => new LongConstant(file.readLong)
			case DataType.DoubleTyp => new DoubleConstant(file.readDouble)
			case DataType.StringTyp => new StringConstant(file.readUTF)
	    case DataType.BoolTyp => new BoolConstant(file.readBoolean)
	    case DataType.VectorTyp => new VectorConstant(file.readDouble,file.readDouble,file.readDouble)
	    case DataType.CurrencyTyp => new CurrencyConstant(file.readLong)
	    case DataType.BlobTyp =>BlobConstant(file)
	    case DataType.DateTyp =>DateConstant(file)
			case _ => EMPTY_EX
		}		
	}
	
	lazy val NullINT=new IntConstant(0) with NullConstant
	lazy val NullLONG=new LongConstant(0) with NullConstant
	lazy val NullDOUBLE=new DoubleConstant(0d) with NullConstant
	lazy val NullSTRING=new StringConstant("") with NullConstant
	lazy val NullBOOL=new BoolConstant(false) with NullConstant
	lazy val NullVECTOR=new VectorConstant(0,0,0) with NullConstant
	lazy val NullCURRENCY=new CurrencyConstant(0) with NullConstant
	lazy val NullBLOB=new BlobConstant(new Array[Byte](0)) with NullConstant
	lazy val NullDATE=new DateConstant(new Date(0))
	
	def generateNullConstant(typ:DataType.Value)= {
		typ match {
			case DataType.IntTyp => NullINT
			case DataType.LongTyp => NullLONG
			case DataType.DoubleTyp => NullBOOL
			case DataType.StringTyp => NullSTRING
	    case DataType.BoolTyp => NullBOOL
	    case DataType.VectorTyp => NullVECTOR
	    case DataType.CurrencyTyp => NullCURRENCY
	    case DataType.undefined => EMPTY_EX
	    case DataType.EnumTyp => NullINT
	    case DataType.DateTyp => NullDATE
	    case DataType.BlobTyp  => NullBLOB
		}
	}
}




