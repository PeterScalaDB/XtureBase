/**
 * Author: Peter Started:10.04.2011
 */
package definition.expression

import definition.typ._
import java.io._

/**
 * 
 */
case class ParentFieldRef(ownerIx:Byte,fieldNr:Byte,var cachedValue:Constant=EMPTY_EX) extends Expression {

  def getType(): DataType.Value = DataType.ParentRefTyp

  def getValue(): Constant = cachedValue

  def createCopy(): Expression = new ParentFieldRef(ownerIx,fieldNr,cachedValue)
  
  def setValue(newValue:Constant)= cachedValue=newValue

  def getChildCount(): Int =  0 

  def getChildNr(ix: Int): Expression =  null

  def getTerm(): String = "#P"+ownerIx+"F"+fieldNr

  def isConstant(): Boolean =  false

  def write(file: DataOutput): Unit = {
  	file.writeByte(DataType.ParentRefTyp.id)
  	file.writeByte(ownerIx)
  	file.writeByte(fieldNr)
  	cachedValue.write(file)
  }
  
  override def toString():String = {
  	getTerm+(if (cachedValue.isNullConstant) "" else " cv:"+cachedValue)
  }
  
  override def equals(other: Any): Boolean =
	other match {
		case that: ParentFieldRef =>
		(that canEqual this) &&
		ownerIx == that.ownerIx &&
		fieldNr == that.fieldNr 
		case _ => false
}

override def hashCode: Int =  41*( 41 + ownerIx.hashCode)  + fieldNr.toInt+1

}



object ParentFieldRef {
	def apply (file: DataInput):Expression = {
		new ParentFieldRef(file.readByte,file.readByte,Expression.readConstant(file))
	}
}