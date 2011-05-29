/**
 * Author: Peter Started:27.05.2011
 */
package client.dataviewer.view3D

import definition.data.{Reference,InstanceData,OwnerReference}
import client.comm._
import definition.comm._
import definition.expression.{Line3D,VectorConstant}
import definition.typ.AllClasses
import java.io.DataInput
import java.awt.{Color,Font}
import javax.media.j3d._
import javax.vecmath._


/**
 * 
 */
class AxisShape(val plane:PlaneElement,volPris:VolumeBranchGroup) extends RefShape(plane.ref,plane.owners(0)) {	
	
  /*def getTopHitPoints(plane:PlaneElement,volPris:VolumePrism)=for(i<-0 until volPris.topEdges.size;
			val firstPoint=if(i>0) i-1 else volPris.topEdges.size-1;
			val edge=volPris.topEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(VectorConstant(volPris.topVertices(firstPoint)),VectorConstant(volPris.topVertices(i)))))
				yield(hitPoint)
				
  def getBottomHitPoints(plane:PlaneElement,volPris:VolumePrism)=for(i<-0 until volPris.bottomEdges.size;
			val firstPoint=if(i>0) i-1 else volPris.topEdges.size-1;
			val edge=volPris.bottomEdges(i);
			if(!plane.isLinearyDependentFrom(edge));
			val hitPoint=plane.intersectionWith(edge);			
			if(hitPoint.isInSegment(VectorConstant(volPris.bottomVertices(firstPoint)),VectorConstant(volPris.bottomVertices(i)))))
				yield(hitPoint)
				
  def getVerticalHitPoints(plane:PlaneElement,volPris:VolumePrism)=for(i<-0 until volPris.wallPlanes.size;
			val secondIx=if(i<(volPris.wallPlanes.size-1))i+1 else 0; 
			val vertEdge=volPris.wallPlanes(i).intersectionWith(volPris.wallPlanes(secondIx))
			if(!plane.isLinearyDependentFrom(vertEdge));
			val hitPoint=plane.intersectionWith(vertEdge);			
			if(hitPoint.isInSegment(VectorConstant(volPris.bottomVertices(i)),VectorConstant(volPris.topVertices(i)))))
  	    yield(hitPoint)*/
  	    
  val lineCoords:Array[Point3d]={
			val topHitPoints=volPris.geometryPrism.getTopHitPoints(plane)
			val vertHitPoints=volPris.geometryPrism.getVerticalHitPoints(plane)
			val bottomHitPoints=volPris.geometryPrism.getBottomHitPoints(plane)
			//println("schräg top:"+topHitPoints.mkString(",")+"\n vert:"+vertHitPoints.mkString+"\n bottom:"+bottomHitPoints.mkString(","))
			val verts=topHitPoints++vertHitPoints++(bottomHitPoints.reverse)
			if( verts.size<3) Array()
			else {
				val testVect=verts(1)-verts(0)
				val restVerts=verts.drop(2).sortWith((avect,bvect)=> (avect-verts(1)).angleBetween(testVect)<(bvect-verts(1)).angleBetween(testVect) )
				(verts.take(2)++restVerts:+verts(0)).map(_.toPoint).toArray
			}
		}
		
		if(lineCoords.size>0) {
			//println("axisplane coords:"+lineCoords.mkString(","))
			val lineStrips=new LineStripArray(lineCoords.size,GeometryArray.COORDINATES,
				Array(lineCoords.size))
			lineStrips.setCoordinates(0,lineCoords)				
			setGeometry(lineStrips)	
		}
		
		val centerPoint=SubVolume.getCenterPoint(lineCoords.dropRight(1))
			
		val appearance=new Appearance()
		val color=new Color(plane.color )
		val colorf=new Color3f(color)
		val colorA=new ColoringAttributes(colorf,ColoringAttributes.NICEST)
		val transA=new TransparencyAttributes(TransparencyAttributes.NICEST,0.4f)
		appearance.setColoringAttributes(colorA)
		appearance.setTransparencyAttributes(transA)
		setAppearance(appearance)	
}


class AxisPlane(lineShape:AxisShape) extends RefShape(lineShape.plane.ref,lineShape.plane .owners(0)){
	//println("create Axis plane ")
	val fanArray=new TriangleFanArray(lineShape.lineCoords.size,GeometryArray.COORDINATES,Array(lineShape.lineCoords.size))
	fanArray.setCoordinates(0,lineShape.lineCoords)
	setGeometry(fanArray)
	val appearance=new Appearance()		
	val colorA=new ColoringAttributes(lineShape.colorf,ColoringAttributes.NICEST)
	val transA=new TransparencyAttributes(TransparencyAttributes.NICEST,0.8f)
	appearance.setColoringAttributes(colorA)
	appearance.setTransparencyAttributes(transA)
	appearance.setPolygonAttributes(SubVolume.axisPlanePolygonAttributes)
	setAppearance(appearance)	
}

class AxisNormVector(lineShape:AxisShape) extends RefShape(lineShape.plane.ref,lineShape.plane .owners(0)){
	
	val pointArray=Array(lineShape.plane.pos.toPoint,(lineShape.plane.pos+lineShape.plane.dir).toPoint)
	val linArray=new LineArray(2,GeometryArray.COORDINATES|GeometryArray.COLOR_3)
	linArray.setCoordinates(0,pointArray)
	linArray.setColors(0,Array(lineShape.colorf,lineShape.colorf))
	setGeometry(linArray)
}
