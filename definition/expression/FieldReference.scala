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
case class FieldReference(remType:Option[Int],remInst:Option[Int],remField:Byte,var cachedValue:Constant=EMPTY_EX) extends Expression {


	lazy val term:String = { 
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

def getNative=toString

override def toString():String = "Ref["+remType+","+remInst+","+remField+",cv:"+cachedValue+"]"


// overrides equals to allowing compare 2 FieldReference objects with different cache values
override def equals(other: Any): Boolean =
	other match {
		case that: FieldReference =>
		(that canEqual this) &&
		remType == that.remType &&
		remInst == that.remInst &&
		remField == that.remField
		case _ => false
}

override def hashCode: Int = 41 * ( 41 + remType.hashCode)  + 1013*(3+remInst.hashCode)+remField.toInt


def isConstant(): Boolean = { false }

def write(file: DataOutput): Unit = {
	file.writeByte(DataType.FieldRefTyp.id)
	remType match {
		case Some (t) => file.writeInt(t)
		case _ => file.writeInt(0)
	}
	remInst match {
		case Some(i) => file.writeInt(i)
		case _ => file.writeInt(0)
	}
	file.writeByte(remField)
	//println("write "+toString+" cached Value:"+cachedValue)
	cachedValue.write(file)
}

def setCachedValue(newVal:Constant) = {
	//print("SetCachedValue "+toString+" oldval:"+cachedValue)
	cachedValue=newVal
	//println(" newval: "+cachedValue)
}



}


object FieldReference {
	def apply (file: DataInput):Expression = {
		val t=file.readInt
		val i=file.readInt
		val f=file.readByte
		val ret =new FieldReference(if(t==0)None else Some(t),if(i==0)None else Some(i),f,Expression.read(file).asInstanceOf[Constant])
		//println(" read "+ret+" cv: "+ret.cachedValue )
		ret
	}
}
