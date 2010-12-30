/**
 * Author: Peter Started:30.11.2010
 */
package definition.expression

import definition.typ._
import java.io._
import java.util.{Currency,Locale}


/**
 * 
 */
case class CurrencyConstant(n:Long) extends Constant {
	lazy val doubleValue=n.toDouble/100d
  def getType(): DataType.Value = { DataType.CurrencyTyp }
  

  def createCopy(): Expression = { new CurrencyConstant(n) }

  def getTerm() = { "%.2f".format(doubleValue)+CurrencyConstant.currencySign }
  
  override def toString=getTerm 
  
  def toInt =  doubleValue.round.toInt
  
  def toLong =  doubleValue.round.toLong
  
  def toDouble = doubleValue
  
  def toBoolean= n>0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.CurrencyTyp.id)
  	//System.out.println("DoubleID:" +DataType.DoubleTyp.id)
  	file.writeLong(n)
  }
  
  def getNative= doubleValue
  
  override def toCurrency=this  
  
  override def isNumberConstant=true 
}

object ImBroke extends CurrencyConstant(0)

object CurrencyConstant {
	lazy val currencySign=java.util.Currency.getInstance(Locale.getDefault). getSymbol()
}