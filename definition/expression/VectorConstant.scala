/**
 * Author: Peter Started:04.10.2010
 */
package definition.expression



import definition.typ.DataType
import java.io.{DataInput,DataOutput}
import javax.vecmath._

/** a 3D Vector
 * 
 */
case class VectorConstant(jvector:Vector3d=new Vector3d) extends Constant {
	
	def this(nx:Double,ny:Double,nz:Double) = {
		this(new Vector3d(nx,ny,nz))
	}
	def x=jvector.x
	def y=jvector.y
	def z=jvector.z
	
	import VectorConstant._
  def getType(): DataType.Value = { DataType.VectorTyp }

  def createCopy(): Expression = { new VectorConstant(new Vector3d(jvector)) }

  def getTerm() = { "V["+x+";"+y+";"+z+"]"}
  
  def shortToString= "["+x+";"+y+";"+z+"]"
  
  override def toString= getTerm
  
  
  
  def toInt =  toDouble.toInt
  
  def toDouble = jvector.length
  
  def toLong = toDouble.toLong
  
  def toBoolean= x!=0 ||y !=0 || z!=0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.VectorTyp.id)
  	file.writeDouble(x)
  	file.writeDouble(y)
  	file.writeDouble(z)
  }
  
  def isNull=(jvector.x==0)&&(jvector.y==0)&&(jvector.z==0)
  
  
  def getNative=shortToString
  
  override def toVector=this
  
  //******************************* Vector routines  ************************************************
  
  def +(other:VectorConstant)= new VectorConstant(x+other.x,y+other.y,z+other.z)
  
  def +(ox:Double,oy:Double,oz:Double)=new VectorConstant(x+ox,y+oy,z+oz)
  
  def -(other:VectorConstant)= new VectorConstant(x-other.x,y-other.y,z-other.z)
  
  //  scale
  def *(scale:Double)= new VectorConstant(x*scale,y*scale,z*scale)
  
  // scalar product
  def *(other:VectorConstant):Double= x*other.x+y*other.y+z*other.z
  
  def unit:VectorConstant= {
  	val length=toDouble
  	if(length==0) return NULLVECTOR
  	else return *(1/length)
  }
  
  def angleBetween(withOther:VectorConstant):Double = Math.acos( *(withOther) / (toDouble*withOther.toDouble) )*180/Math.Pi 
  
  def norm2d:VectorConstant = new VectorConstant(-y,x,0).unit 
  
    
  def cross(other:VectorConstant):VectorConstant = 
  	new VectorConstant(y*other.z-z*other.y, z*other.x-x*other.z,x*other.y-y*other.x	)
  
  
  
  def squareDistanceTo(ox:Double,oy:Double,oz:Double):Double = {
  	val dx=x-ox
  	val dy=y-oy
  	val dz=y-oy
  	dx*dx+dy*dy+dz*dz
  }  
  
  def compareTo(nob:VectorConstant):Int =  {       
  	if(x>nob.x) return 1;
  	if(x<nob.x) return -1;
  	if(y>nob.y) return 1;
  	if(y<nob.y) return -1;
  	if(z>nob.z) return 1;
  	if(z<nob.z) return -1;
  	return 0;  
  }  
  
  def orthoProjection(toVector:VectorConstant):VectorConstant=   {
    if(toVector.toDouble==0) return toVector;
    val skal= *(toVector)/(toVector*toVector)
    return toVector* skal
  }
  
  /** get a vector that is orthogonal to this and points to fromVector
   * 
   * @param fromVector
   * @return
   */
  def orthogonalThrough(fromVector:VectorConstant)= this.-(orthoProjection(fromVector)) 
  
  /** is this vector lineary dependent with the other vector
   * 
   * @param other the other vector
   * @return true if both vectors are lineary dependent
   */
  def isLinearyDependentFrom(other:VectorConstant) =    	
  	det2D(other.x,other.y,x,y)==0 && det2D(other.x,other.z,x,z)==0 && det2D(other.y,other.z,y,z)==0
  
  
}

/**
 * @param pos Position of the directional vector
 * @param dir Directional vector of the line
 */
case class Line3D(pos:VectorConstant,dir:VectorConstant){
	import VectorConstant._
	
	def intersectionWith(other:Line3D):VectorConstant= {
		val dif=other.pos-pos
  	if(dir.isLinearyDependentFrom(other.dir)) 
  			throw new ArithmeticException("Intersection not possible, vectors are linerary dependent "+this+" "+other);
  	val det=VectorConstant.determinant(dir,other.dir,dif)
  	if(det==0) { // there is an intersection  	
  		val det=det2D(dir.x,dir.y,other.dir.x,other.dir.y)
  		if(det!=0){
  			val det1=det2D(dif.x,dif.y,other.dir.x,other.dir.y)  			
  			return pos +(dir *(det1/det))
  		} else {
  			val det=det2D(dir.x,dir.z,other.dir.x,other.dir.z)
  			if(det!=0) {
  				val det1=det2D(dif.x,dif.z,other.dir.x,other.dir.z)  				
  				return pos +(dir *(det1/det))
  			}	else {
  				val det=det2D(dir.y,dir.z,other.dir.y,other.dir.z)
  				if(det!=0)  				{
  					val det1=det2D(dif.y,dif.z,other.dir.y,other.dir.z)  					
  					return pos + (dir *(det1/det))
  				}  				 
  			}
  		}  		
  	}
  	throw new ArithmeticException("Cant find intersection between "+this+" and "+other);
	}	
	
	/** orthogonal projecion from point to this line
	 * 
	 * @param point a point in space
	 * @return the projection point on this line
	 */
	def orthProjection(point:VectorConstant)=		
		pos + (point - pos).orthoProjection(dir)
	
	/** get a vector that is orthogonal to this and points to point
	 * 
	 * @param point
	 * @return
	 */
	def orthogonalThrough(point:VectorConstant)= 
		point-orthProjection(point)
		
	def distanceTo(point:VectorConstant)=Math.abs(orthogonalThrough(point).toDouble)
	
	def isLinearDependent(other:Line3D)= dir.isLinearyDependentFrom(other.dir)
}

object VectorConstant {
	def det2D(a11:Double,a21:Double,a12:Double,a22:Double)=   {
    a11*a22-a12*a21
  }
	
	def determinant(a:VectorConstant,b:VectorConstant,c:VectorConstant) =   {
    a.x*b.y*c.z - a.x*c.y*b.z - a.y*b.x*c.z + a.y*c.x*b.z + a.z*b.x*c.y - a.z*c.x*b.y;
  }
}

object NULLLINE extends Line3D(NULLVECTOR,NULLVECTOR)

object NULLVECTOR extends VectorConstant()

