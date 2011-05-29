/**
 * Author: Peter Started:22.05.2011
 */
package client.dataviewer.view3D
import definition.expression.{VectorConstant,Line3D}
import definition.data.Reference

/**
 * 
 */
trait Prism {	
  def topPlane:PlaneElement
	def bottomPlane:PlaneElement	
	def wallPlanes:Seq[PlaneElement]
	// main geometry
  //println("before "+wallPlanes)
	lazy val topEdges:Seq[Line3D]=wallPlanes map (_.intersectionWith(topPlane))
	lazy val bottomEdges:Seq[Line3D]=wallPlanes map (_.intersectionWith(bottomPlane))
	lazy val topVertices=for(i<-wallPlanes.indices;
		val secondIx=if(i<(wallPlanes.size-1))i+1 else 0) 
		yield wallPlanes(i).intersectionWith(topEdges(secondIx))
	
	lazy val bottomVertices=for(i<-wallPlanes.indices;val secondIx=if(i<(wallPlanes.size-1))i+1 else 0) 
		yield wallPlanes(i).intersectionWith(bottomEdges(secondIx))
	
	lazy val topPoints=topVertices.map(_.toPoint)
	lazy val bottomPoints=bottomVertices.map(_.toPoint)
	lazy val topCenter=SubVolume.getCenterPoint(topPoints)
	lazy val bottomCenter=SubVolume.getCenterPoint(bottomPoints)
	lazy val centerPoint=SubVolume.getCenterPoint(List(topCenter,bottomCenter))	
	lazy val centerP=new VectorConstant(centerPoint)
  
  def getTopHitPoints(plane:PlaneElement)=for(i<-0 until topEdges.size;
			val firstPoint=if(i>0) i-1 else topEdges.size-1;
			val edge=topEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(topVertices(firstPoint),topVertices(i))))
				yield(hitPoint)
				
  def getBottomHitPoints(plane:PlaneElement)=for(i<-0 until bottomEdges.size;
			val firstPoint=if(i>0) i-1 else topEdges.size-1;
			val edge=bottomEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(bottomVertices(firstPoint),bottomVertices(i))))
				yield(hitPoint)
	
	def getTopHitPointsWithIndex(plane:PlaneElement)=for(i<-0 until topEdges.size;
			val firstPoint=if(i>0) i-1 else topEdges.size-1;
			val edge=topEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(topVertices(firstPoint),topVertices(i))))
				yield (hitPoint,i)
				
  def getBottomHitPointsWithIndex(plane:PlaneElement)=for(i<-0 until bottomEdges.size;
			val firstPoint=if(i>0) i-1 else topEdges.size-1;
			val edge=bottomEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(bottomVertices(firstPoint),bottomVertices(i))))
				yield (hitPoint,i)			
				
  def getVerticalHitPoints(plane:PlaneElement)=for(i<-0 until wallPlanes.size;
			val secondIx=if(i<(wallPlanes.size-1))i+1 else 0; 
			val vertEdge=wallPlanes(i).intersectionWith(wallPlanes(secondIx))
			if(!plane.isLinearyDependentFrom(vertEdge));
			val hitPoint=plane.intersectionWith(vertEdge);			
			if(hitPoint.isInSegment(bottomVertices(i),topVertices(i))))
  	    yield(hitPoint) 
}


class DataPrism(val ref:Reference,val planeList:Seq[PlaneElement]) extends Prism  {
	val topPlane=planeList(0)
	val bottomPlane=planeList(1)
	val wallPlanes=planeList.drop(2)
}