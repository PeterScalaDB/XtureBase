/**
 * Author: Peter Started:04.10.2010
 */
package client.graphicsView

import client.comm.{SubscriptionFactory,ReadableClass}
import definition.data.{Reference,InstanceData}
import definition.expression.{VectorConstant,Expression}
import definition.typ.AllClasses
import java.awt.geom.{Rectangle2D}
import java.io.DataInput

/** super class for all graphical elements
 * 
 */
class GraphElem(val ref:Reference,val color:Int,val lineStyle:Int) extends ReadableClass {
  def getBounds:Rectangle2D	=new Rectangle2D.Double()
}

class LineElement(nref:Reference,ncolor:Int,nlineStyle:Int,startPoint:VectorConstant,endPoint:VectorConstant) extends 
		GraphElem(nref,ncolor,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(Math.min(startPoint.x,endPoint.x),Math.min(startPoint.y,endPoint.y),
		Math.max(startPoint.x,endPoint.x),Math.max(startPoint.y,endPoint.y))
	override def getBounds=bounds
	override def toString= "Line ("+startPoint.shortToString+","+endPoint.shortToString+", Col:"+color+", Style:"+lineStyle+")"
}

object GraphElemFactory extends SubscriptionFactory[GraphElem] {
	def emptyFunc(ref:Reference)= new GraphElem(ref,0,0)
	
	registerClass(AllClasses.get.getClassIDByName("LineElem"),createLine)
	
	def createLine (ref:Reference,in:DataInput) = {
		val nfields=in.readByte
		if(nfields!=4) println("wrong number of fields")
		val color=Expression.readConstant(in)
		val lineStyle=Expression.readConstant(in)
		val startPoint=Expression.readConstant(in).toVector
		val endPoint=Expression.readConstant(in).toVector
		val owners=InstanceData.readOwners(in)
		//print("owners:"+owners.size+" "+owners.mkString)
		//print(" hasChildren:"+in.readBoolean)
		new LineElement(ref,color.toInt,lineStyle.toInt,startPoint,endPoint)
	}
}