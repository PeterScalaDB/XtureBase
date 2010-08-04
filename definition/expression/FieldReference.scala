/**
 * Author: Peter Started:03.08.2010
 */
package definition.expression

import definition.typ._
import java.io._

/** Expresses a reference to the value of another field in this or another object
 * @field remType optional type of the remote object. can be ommittet when the remote object
 * has the same type as this one
 * @field remInst optional instance of the remote object. can be ommitted when the referred field
 * is in the same instance  
 */
case class FieldReference(remType:Option[Int],remInst:Option[Long],remField:Byte,var cachedValue:Constant=EMPTY_EX) extends Expression {
	

	val term:String = { 
	"#" + (remType match {
		case Some(t) => "T"+ t.toString
		case _ => "" }) + 
		(remInst match {
			case Some(i) => "I"+ i.toString
			case _ => "" 
		}) + "F"+remField.toString 
}

def getType(): DataType.Value = { DataType.FieldRefTyp }

def getValue(): Constant = { cachedValue }

def createCopy(): Expression = {
	new FieldReference(remType,remInst,remField,cachedValue)
	
}

def getChildCount(): Int = { 0 }

def getChildNr(ix: Int): Expression = { null }

def getTerm(): String = term

override def toString():String = "Ref["+remType+","+remInst+","+remField+"]"

def isConstant(): Boolean = { false }

def write(file: DataOutput): Unit = {
	file.writeByte(DataType.FieldRefTyp.id)
	remType match {
	  case Some (t) => file.writeInt(t)
	  case _ => file.writeInt(0)
	}
	remInst match {
		case Some(i) => file.writeLong(i)
		case _ => file.writeLong(0)
	}
	file.writeByte(remField)
	cachedValue.write(file)
}

override def getFieldReferences(resultList:List[FieldReference])= this :: resultList

}


object FieldReference {
	def apply (file: DataInput):Expression = {
    val t=file.readInt
    val i=file.readLong
    val f=file.readByte
    new FieldReference(if(t==0)None else Some(t),if(i==0)None else Some(i),f,Expression.read(file).asInstanceOf[Constant])
	}
}
