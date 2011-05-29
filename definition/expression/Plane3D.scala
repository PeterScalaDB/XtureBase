/**
 * Author: Peter Started:13.05.2011
 */
package definition.expression

//import java.io._





/** a plane in3D Space
 * @param pos the Position of the plane in 3D Space
 * @param dir the norm vector for the direction of the plane 
 */
class Plane3D(val pos:VectorConstant,val dir:VectorConstant) {
  lazy val dirUnit=dir.unit
	
  //import Plane3D._ 
  
  
  //implicit def builder[A <:Plane3D]:PlaneBuilder[A]=SimpleBuilder
  
	def isLinearyDependentFrom(other:Plane3D)= dir.isLinearyDependentFrom(other.dir)
	
	def isLinearyDependentFrom(line:Line3D)= (dir*line.dir) == 0
	
	def isVertical= dir.z==0
	
	def isHorizontal = dir.x==0 && dir.y==0
	
	def isDefined= !dir.isNull
	
	def getDistanceTo(point:VectorConstant)= (point-pos)*dirUnit
	
	def orthProjection(point:VectorConstant)=	(point-pos)-(dirUnit*getDistanceTo(point))
	
	def orthogonalThrough(point:VectorConstant)= dirUnit*getDistanceTo(point)
	
	def angleBetween(otherDir:VectorConstant)= {
		val ret=90-dir.angleBetween(otherDir)
		if(ret<0) -ret else ret
	}
	
	def angleBetween(other:Plane3D)= dir.angleBetween(other.dir)
	
	
	
	def intersectionWith(line:Line3D):VectorConstant = {
		if(isLinearyDependentFrom(line)) 
			throw new IllegalArgumentException("Find intersection, plane "+this+" is parallel with "+line )
    val r=	-1* (dir*(line.pos-pos))/(dir*line.dir)
    line.pos + line.dir*r
	}
	
	
	def intersectionWith(other:Plane3D):Line3D= {
		if(dir.isLinearyDependentFrom(other.dir))
			throw new IllegalArgumentException("Find intersection, plane "+this+" is parallel with "+other )
		val otherDirProjected=other.dir-dirUnit*(other.dir*dirUnit)
		val startPoint=other.intersectionWith(new Line3D(pos,otherDirProjected))
		new Line3D(startPoint,dir cross other.dir)
	}
	
	def createClone(newPos:VectorConstant,newDir:VectorConstant) = new Plane3D(newPos,newDir)
	
	//def cop(t:VectorConstant,o:Double)= createOffsetPlane(t,o)
	
	
	
	def canEqual(other: Any): Boolean = other.isInstanceOf[Plane3D]
	
	override def equals(other: Any): Boolean =
		other match {
				case that: Plane3D =>
				(that canEqual this) && pos== that.pos && dir==that.dir								
				case _ => false
		}
	
	override def hashCode= pos.hashCode+dir.hashCode*3
	
  
}

/*object Plane3D {
	  implicit def builder[A<:Plane3D] : PlaneBuilder[A]=myBuilder
		 val myBuilder=new PlaneBuilder[Plane3D]{ 
			def createClone(oldv:Plane3D,newPos:VectorConstant,newDir:VectorConstant)= new Plane3D(newPos,newDir)
		}
	}*/