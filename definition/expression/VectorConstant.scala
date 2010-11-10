/**
 * Author: Peter Started:04.10.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/** a 3D Vector
 * 
 */
case class VectorConstant(val x:Double,val y:Double,val z:Double) extends Constant { 
  def getType(): DataType.Value = { DataType.VectorTyp }

  def createCopy(): Expression = { new VectorConstant(x,y,z) }

  def getTerm() = { "V["+x+";"+y+";"+z+"]"}
  
  def shortToString= "["+x+";"+y+";"+z+"]"
  
  override def toString= getTerm
  
  
  
  def toInt =  toDouble.toInt
  
  def toDouble = Math.sqrt(x*x+y*y+z*z)
  
  def toLong = toDouble.toLong
  
  def toBoolean= x!=0 ||y !=0 || z!=0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.VectorTyp.id)
  	file.writeDouble(x)
  	file.writeDouble(y)
  	file.writeDouble(z)
  }
  
  def +(other:VectorConstant)= new VectorConstant(x+other.x,y+other.y,z+other.z)
  
  def +(ox:Double,oy:Double,oz:Double)=new VectorConstant(x+ox,y+oy,z+oz)
  
  def -(other:VectorConstant)= new VectorConstant(x-other.x,y-other.y,z-other.z)
  
  //  scale
  def *(scale:Double)= new VectorConstant(x*scale,y*scale,z*scale)
  
  def unit:VectorConstant= {
  	val length=toDouble
  	return *(1/length)
  }
  
  // scalar product
  def *(other:VectorConstant):Double= x*other.x+y*other.y+z*other.z
  
  def angle(withOther:VectorConstant):Double = Math.acos( *(withOther) / (toDouble*withOther.toDouble) )*180/Math.Pi 
  
  def norm2d:VectorConstant = new VectorConstant(-y,x,0).unit  
  
  def cross(other:VectorConstant):VectorConstant = 
  	new VectorConstant(y*other.z-z*other.y, z*other.x-x*other.z,x*other.y-y*other.x	)
  
  override def toVector=this
  
  def squareDistanceTo(ox:Double,oy:Double,oz:Double):Double = {
  	val dx=x-ox
  	val dy=y-oy
  	val dz=y-oy
  	dx*dx+dy*dy+dz*dz
  }
  def getNative=shortToString
}

object NULLVECTOR extends VectorConstant(0,0,0)

