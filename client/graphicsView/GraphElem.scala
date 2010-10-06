/**
 * Author: Peter Started:04.10.2010
 */
package client.graphicsView

import client.comm.{SubscriptionFactory,ReadableClass}
import definition.data.{Reference,InstanceData}
import definition.expression.{VectorConstant,Expression}
import definition.typ.AllClasses
import java.awt.geom.{Rectangle2D}
import java.awt.{Graphics2D,Color}
import java.io.DataInput

/** super class for all graphical elements
 * 
 */
class GraphElem(val ref:Reference,val color:Int) extends ReadableClass {
  def getBounds:Rectangle2D	=new Rectangle2D.Double()
  def draw(g:Graphics2D,sm:ScaleModel)={}
  def minX=0d
  def maxX=0d
  def minY=0d
  def maxY=0d
}

class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor)

class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,startPoint:VectorConstant,endPoint:VectorConstant) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(Math.min(startPoint.x,endPoint.x),Math.min(startPoint.y,endPoint.y),
		Math.max(startPoint.x,endPoint.x),Math.max(startPoint.y,endPoint.y))
	override def getBounds=bounds
	override def toString= "Line ("+startPoint.shortToString+","+endPoint.shortToString+", Col:"+color+", Style:"+lineStyle+")"
	
	override def draw(g:Graphics2D,sm:ScaleModel)={
		g.setPaint(new Color(color))
		g.drawLine(sm.xToScreen(startPoint.x) ,sm.yToScreen(startPoint.y) ,
			sm.xToScreen(endPoint.x),sm.yToScreen(endPoint.y))	
	}
	override def minX=if(startPoint.x<endPoint.x) startPoint.x else endPoint.x
	override def maxX=if(startPoint.x>endPoint.x) startPoint.x else endPoint.x
	override def minY=if(startPoint.y<endPoint.y) startPoint.y else endPoint.y
	override def maxY=if(startPoint.y>endPoint.y) startPoint.y else endPoint.y
}

object GraphElemFactory extends SubscriptionFactory[GraphElem] {
	def emptyFunc(ref:Reference)= new GraphElem(ref,0)
	
	registerClass(AllClasses.get.getClassIDByName("LineElem"),createLine)
	
	def createLine (ref:Reference,in:DataInput) = {
		val nfields=in.readByte
		if(nfields!=5) println("wrong number of fields")
		val color=Expression.readConstant(in)
		val lineWidth=Expression.readConstant(in)
		val lineStyle=Expression.readConstant(in)
		val startPoint=Expression.readConstant(in).toVector
		val endPoint=Expression.readConstant(in).toVector
		val owners=InstanceData.readOwners(in)
		//print("owners:"+owners.size+" "+owners.mkString)
		//print(" hasChildren:"+)
		in.readBoolean
		new LineElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,startPoint,endPoint)
	}
}