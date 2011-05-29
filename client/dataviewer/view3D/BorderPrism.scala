/**
 * Author: Peter Started:27.05.2011
 */
package client.dataviewer.view3D
import client.comm.{ClientQueryManager}
import definition.comm.NotificationType
import definition.data.{Reference,InstanceData,Referencable}
import definition.typ.SystemSettings
import definition.expression.{VectorConstant, Line3D}
import javax.media.j3d.{Shape3D, LineStripArray, LineArray, GeometryArray, Font3D, Text3D,FontExtrusion,OrientedShape3D,
	Appearance,TransformGroup,Transform3D,LineAttributes,ColoringAttributes,BoundingBox,TransparencyAttributes}
import javax.vecmath.{Color3f, Point3d,Point3f,Vector3f,Vector3d}
import java.awt.{Font,Color}
/**
 * 
 */
class BorderPrism(val nref:Reference,val name:String,val planeList:Seq[PlaneElement]) extends DBBranchGroup(nref) {
	val topPlane=planeList(0)
	val bottomPlane=planeList(1)	
	val northPlane=planeList(2)
	val eastPlane=planeList(3)
	val southPlane=planeList(4)
	val westPlane=planeList(5)
	val northUpEdge=topPlane.intersectionWith(northPlane)
	val southUpEdge=topPlane.intersectionWith(southPlane)
	val northLowEdge=bottomPlane.intersectionWith(northPlane)
	val southLowEdge=bottomPlane.intersectionWith(southPlane)
	val topC=IndexedSeq(westPlane.intersectionWith(northUpEdge).toPoint,
														eastPlane.intersectionWith(northUpEdge).toPoint,
														eastPlane.intersectionWith(southUpEdge).toPoint,
														westPlane.intersectionWith(southUpEdge).toPoint)
  val bottomC=IndexedSeq(westPlane.intersectionWith(northLowEdge).toPoint,
														eastPlane.intersectionWith(northLowEdge).toPoint,
														eastPlane.intersectionWith(southLowEdge).toPoint,
														westPlane.intersectionWith(southLowEdge).toPoint)	
	
	val lineCoords=(topC++List(topC(0))++bottomC++List(bottomC(0))++List(topC(0),bottomC(0),topC(1),bottomC(1),topC(2),bottomC(2),topC(3),bottomC(3))).toArray
														
  val lineStrips=new LineStripArray(18,GeometryArray.COORDINATES,Array(5,5,2,2,2,2))
	lineStrips.setCoordinates(0,lineCoords)
	val cubeShape =new Shape3D(lineStrips)
	/*val dotLineAttrib=new LineAttributes()	
	dotLineAttrib.setLineAntialiasingEnable(true)
	dotLineAttrib.setLineWidth(0.2f)
	dotLineAttrib.setLinePattern(LineAttributes.PATTERN_USER_DEFINED)
	dotLineAttrib.setPatternMask(21845)
	dotLineAttrib.setPatternScaleFactor(2)
	println("borderPrism:"+lineCoords.mkString(","))*/
	val boxAppear=new Appearance()
	//boxAppear.setLineAttributes(dotLineAttrib)
	boxAppear.setTransparencyAttributes(new TransparencyAttributes(TransparencyAttributes.NICEST,0.6f))
	boxAppear.setColoringAttributes(new ColoringAttributes(new Color3f(Color.black),ColoringAttributes.NICEST))
	cubeShape.setAppearance(boxAppear)
	
	val xRay=new Line3D(new VectorConstant(0,0,0),new VectorConstant(1,0,0))
	val yRay=new Line3D(new VectorConstant(0,0,0),new VectorConstant(0,1,0))
	val zRay=new Line3D(new VectorConstant(0,0,0),new VectorConstant(0,0,1))
	
	val axisLinesCoords= Array(westPlane.intersectionWith(xRay).toPoint,eastPlane.intersectionWith(xRay).toPoint,
		southPlane.intersectionWith(yRay).toPoint,northPlane.intersectionWith(yRay).toPoint,
		bottomPlane.intersectionWith(zRay).toPoint,topPlane.intersectionWith(zRay).toPoint)
	val axisLines=new LineArray(6,GeometryArray.COORDINATES| GeometryArray.COLOR_3)
	axisLines.setCoordinates(0,axisLinesCoords)
	val axisShape=new Shape3D(axisLines)
	val blue=new Color3f(0,0,0.6f)
	val lightBlue=new Color3f(0.5f,0.5f,1)
	val red=new Color3f(0.6f,0,0)
	val lightRed=new Color3f(1,0.5f,0.5f)
	val green=new Color3f(0,0.6f,0)
	val lightGreen=new Color3f(0.5f,1,0.5f)
	axisLines.setColors(0,Array(blue,lightBlue,red,lightRed,green,lightGreen))
	val font3d = new Font3D(new Font("Helvetica", Font.PLAIN, 1),new FontExtrusion());
	
	
	
	
	addChild(cubeShape)
	addChild(axisShape)
	addChild(WorkArea.createAxisText("X",axisLinesCoords(1),font3d,lightBlue,0.7f))
	addChild(WorkArea.createAxisText("Y",axisLinesCoords(3),font3d,lightRed,0.7f))
	addChild(WorkArea.createAxisText("Z",axisLinesCoords(5),font3d,lightGreen,0.7f))
	
	override def toString= {
		"BorderPrism \ntopCorners"+topC.mkString(",")+"\nbottomCorners:"+bottomC.mkString(",")
	}
	
	
	
}
