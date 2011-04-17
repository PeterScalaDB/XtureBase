/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import definition.typ.DataType

/** Abstract trait of all operators for binary operations
 * 
 */
abstract class BinOperator(val opChar:Char,val level:Byte) {
	def getValue(left:Constant,right:Constant):Option[Constant]
	override def toString=opChar.toString
}

object BinOperator
{
	/** finds the operator object with the given char
	 * 
	 * @param op name of the operator to find
	 */
	def getOp(op:Char) =
	{
		if (oList contains op) oList(op)
		else throw new IllegalArgumentException("Operator "+op+" is unknown")
	}
	
	// constant 1 is numberConstant
	def doCalculation(c1:Constant,c2:Constant,func:(Double,Double)=>Number):Constant= {
		//System.out.println("do calculation c1:"+c1+" "+c1.getType+" c2:"+c2+" "+c2.getType+" func:"+func)
		if(c2.isNumberConstant) { // both are Number constants
			if(c1.getType==DataType.CurrencyTyp&&c2.getType==DataType.CurrencyTyp)
				new CurrencyConstant(Math.round(func(c1.toDouble,c2.toDouble).doubleValue*100L))
			else if(c1.getType==DataType.CurrencyTyp||c2.getType==DataType.CurrencyTyp) 
				new CurrencyConstant(Math.round(func(c1.toDouble,c2.toDouble).doubleValue*100L))
			else if(c1.getType==DataType.DoubleTyp||c2.getType==DataType.DoubleTyp)
				new DoubleConstant(func(c1.toDouble,c2.toDouble).doubleValue)
			else if(c1.getType==DataType.LongTyp||c2.getType==DataType.LongTyp)
				new LongConstant(func(c1.toLong,c2.toLong).longValue)
			else new IntConstant(func(c1.toInt,c2.toInt).intValue)
		} else {
			c1.getType match {
				case DataType.IntTyp => new IntConstant (func(c1.toInt,c2.toInt).intValue)
  			case DataType.LongTyp => new LongConstant (func(c1.toLong,c2.toLong).longValue)
  			case DataType.DoubleTyp => new DoubleConstant (func(c1.toDouble,c2.toDouble).doubleValue)
  			case DataType.CurrencyTyp => new CurrencyConstant (Math.round(func(c1.toDouble,c2.toDouble).doubleValue*100L))
			}
		}
	}
	
	def add(a:Double,b:Double)=a+b
	def sub(a:Double,b:Double)=a-b
	def mul(a:Double,b:Double)=a*b
	def div(a:Double,b:Double)=a/b
	def pow(a:Double,b:Double)=Math.pow(a,b)
	
	def multString(st:String, num:Int)= {
		if(num<2)st else {
		val sb=	new StringBuilder();
		for(i<- 1 to num) {
			sb.append(st);
		}
		sb.toString()
		}			
	}
	
	private val oList=Map (
		('+' -> new BinOperator('+',1){
			def getValue(left:Constant,right:Constant)= {
				
				if(left.isNumberConstant) Some(doCalculation(left,right,add))
				else left.getType match 
  			{  				
  				case DataType.StringTyp => Some(new StringConstant (left.toString + right.toString ))
  				case DataType.VectorTyp => if(right.getType==DataType.VectorTyp) {
  					Some(left.asInstanceOf[VectorConstant]+right.asInstanceOf[VectorConstant])  					
  					
  				} else throw new IllegalArgumentException( "Cant add Datatype "+left.getType+" to Vectors ")
  				case DataType.undefined => Some(right)
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for add operation ")
  			} 			
			}
		}),
		('-' -> new BinOperator('-',1){
			def getValue(left:Constant,right:Constant)= {				
				if(left.isNumberConstant) Some(doCalculation(left,right,sub))
				else left.getType match   			
				{
  				case DataType.IntTyp => Some(new IntConstant (left.toInt -right.toInt))
  				case DataType.LongTyp => Some(new LongConstant (left.toLong -right.toLong))
  				case DataType.DoubleTyp => Some(new DoubleConstant (left.toDouble - right.toDouble ))
  				case DataType.VectorTyp => if(right.getType==DataType.VectorTyp) {
  					Some(left.asInstanceOf[VectorConstant]-right.asInstanceOf[VectorConstant])
  				} else throw new IllegalArgumentException( "Cant add Datatype "+left.getType+" to Vectors ")
  				case DataType.undefined => Some(new DoubleConstant(0 - right.toDouble))
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for sub operation ")
  			} 			
			}
		}),
		('*' -> new BinOperator('*',2){
			def getValue(left:Constant,right:Constant)= {
				if(left.isNumberConstant) Some(doCalculation(left,right,mul))
				else
				left.getType match 
  			{
					case DataType.StringTyp => Some(new StringConstant ( multString( left.toString, right.toInt) ))
  				case DataType.VectorTyp => {
  					val leftVector=left.asInstanceOf[VectorConstant]
  					if(right.getType==DataType.VectorTyp) 
  						Some(DoubleConstant(leftVector*right.asInstanceOf[VectorConstant]))
  					else Some(leftVector*right.toDouble)  					
  				}
  				case DataType.undefined => Some(Expression.NullDOUBLE)	 
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for mult operation "+left+" "+left.getTerm)
  			} 			
			}
		}),
		('/' -> new BinOperator('/',2){
			def getValue(left:Constant,right:Constant)= {
				if(left.isNumberConstant) Some(doCalculation(left,right,div))
				else
				left.getType match 
  			{
					case DataType.undefined => Some(Expression.NullDOUBLE)
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for div operation ")
  			} 
				
			}
		})
		,
		('^' -> new BinOperator('^',3){
			def getValue(left:Constant,right:Constant)= {
				if(left.isNumberConstant) Some(doCalculation(left,right,pow))
				else
				left.getType match 
  			{  				
					case DataType.undefined => Some(Expression.NullDOUBLE)
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for pow operation ")
  			} 			
			}
		}),
		('=' -> new BinOperator('=',0){
			def getValue(left:Constant,right:Constant)= {
				Some(new BoolConstant(left.getValue==right.getValue))			
			}
		}),
		('<' -> new BinOperator('<',0){
			def getValue(left:Constant,right:Constant)= {				
				left.getType match {
				case DataType.StringTyp => Some(new BoolConstant(left.getValue.toString<right.getValue.toString))				
				case DataType.DoubleTyp|DataType.CurrencyTyp  => Some(new BoolConstant(left.getValue.toDouble<right.getValue.toDouble))
				case _ => Some(new BoolConstant(left.getValue.toInt<right.getValue.toInt))
				}							
			}
		}),
		('>' -> new BinOperator('>',0){
			def getValue(left:Constant,right:Constant)= {				
				left.getType match {
				case DataType.StringTyp => Some(new BoolConstant(left.getValue.toString>right.getValue.toString))				
				case DataType.DoubleTyp|DataType.CurrencyTyp  => Some(new BoolConstant(left.getValue.toDouble>right.getValue.toDouble))
				case _ => Some(new BoolConstant(left.getValue.toInt>right.getValue.toInt))
				}							
			}
		})
	)
}