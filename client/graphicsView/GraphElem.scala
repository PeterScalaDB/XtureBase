/**
 * Author: Peter Started:04.10.2010
 */
package client.graphicsView

import client.comm.{SubscriptionFactory}
import definition.data.{Reference,InstanceData,Referencable}
import definition.expression.{VectorConstant,Expression}
import definition.typ.AllClasses
import java.awt.geom.{Rectangle2D,Arc2D,Line2D}
import java.awt.{Graphics2D,Color}
import java.io.{DataInput,DataOutput}

/** super class for all graphical elements
 * 
 */
class GraphElem(override val ref:Reference,val color:Int) extends Referencable {
  def getBounds:Rectangle2D	=new Rectangle2D.Double()
  def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)={}
  def minX=0d
  def maxX=0d
  def minY=0d
  def maxY=0d
  
  def hits(px:Double,py:Double,dist:Double)=false // hittest  
  def hitPoint(px:Double,py:Double,dist:Double):Option[VectorConstant]=None
}

class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor)

case class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,startPoint:VectorConstant,endPoint:VectorConstant) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(Math.min(startPoint.x,endPoint.x),Math.min(startPoint.y,endPoint.y),
		Math.max(startPoint.x,endPoint.x),Math.max(startPoint.y,endPoint.y))
	override def getBounds=bounds
	override def toString= "Line ("+startPoint.shortToString+","+endPoint.shortToString+", Col:"+color+", Style:"+lineStyle+")"
	
	override def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)={
		g.setPaint(if(selectColor==null)new Color(color)else selectColor)
		g.setStroke(sm.getStroke(lineWidth))
		g.drawLine(sm.xToScreen(startPoint.x) ,sm.yToScreen(startPoint.y) ,
			sm.xToScreen(endPoint.x),sm.yToScreen(endPoint.y))	
	}
	override def minX=if(startPoint.x<endPoint.x) startPoint.x else endPoint.x
	override def maxX=if(startPoint.x>endPoint.x) startPoint.x else endPoint.x
	override def minY=if(startPoint.y<endPoint.y) startPoint.y else endPoint.y
	override def maxY=if(startPoint.y>endPoint.y) startPoint.y else endPoint.y
	
	override def hits(px:Double,py:Double,dist:Double)= {
		val calcDist=GraphElemFactory.getLineDistance(startPoint.x,startPoint.y,endPoint.x,endPoint.y,px,py)
		//println("hittest startp:"+startPoint+" dist:"+calcDist)
		calcDist<=dist && px>=(minX-dist) && (px<=(maxX)+dist) &&
		  py>=(minY-dist) && (py<=(maxY)+dist)
	}
	override def hitPoint(px:Double,py:Double,dist:Double)= {
		var ret:Option[VectorConstant]=None
		//println("test x:"+(px-startPoint.x)+ " y:"+(py-startPoint.y))
		if(Math.abs(px-startPoint.x)<dist&& Math.abs(py-startPoint.y)<dist) ret=Some(startPoint)
		if(Math.abs(px-endPoint.x)<dist&& Math.abs(py-endPoint.y)<dist) {
			if(ret.isDefined) { // startPoint and endPoint are in distance
				// check wich point is nearer				
				val sdist=startPoint.squareDistanceTo(px,py,0)				
				val edist=endPoint.squareDistanceTo(px,py,0)
				if(edist<sdist) ret=Some(endPoint)
			}
			else ret= Some(endPoint)
		}
		//if(ret.isDefined) println("ret="+ret)
		ret
	}
}


case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
	diameter:Double,startAngle:Double,endAngle:Double) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(centerPoint.x-diameter,centerPoint.y-diameter,
		centerPoint.x+diameter,centerPoint.y+diameter)
	override def getBounds=bounds
	override def toString= "Arc ("+centerPoint.shortToString+") d="+diameter+", sa:"+startAngle+", eA:"+endAngle+")"
	
	override def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)={
		g.setPaint(if(selectColor==null)new Color(color)else selectColor)
		g.setStroke(sm.getStroke(lineWidth))
		
		val tx=sm.xToScreen(centerPoint.x-diameter)
		val ty=sm.yToScreen(centerPoint.y+diameter)
		GraphElemFactory.theArc.setArc(tx.toDouble,ty.toDouble ,sm.xToScreen(centerPoint.x+diameter)-tx,
			sm.yToScreen(centerPoint.y-diameter)-ty,
			startAngle, ((if(endAngle<startAngle)360 else 0)+endAngle-startAngle),Arc2D.OPEN)
		g.draw(GraphElemFactory.theArc)
	}
	override def minX= centerPoint.x-diameter
	override def maxX= centerPoint.x+diameter
	override def minY= centerPoint.y-diameter
	override def maxY= centerPoint.y+diameter
	
	override def hits(px:Double,py:Double,dist:Double):Boolean= {
		val dx=px-centerPoint.x
		val dy=py-centerPoint.y
		val pd=Math.sqrt(dx*dx+dy*dy)
		if(Math.abs(pd-diameter)>dist) return false
		var angle=Math.atan2(dy,dx)*180/Math.Pi
		if(angle<0) angle=360+angle
		println("Hittest angle:"+angle+" sa:"+startAngle+" ea:"+endAngle)
		if(startAngle<endAngle) return (angle>=startAngle)&&(angle<=endAngle)
		else return (angle>=startAngle)||(angle<=endAngle)
	}
}





object GraphElemFactory extends SubscriptionFactory[GraphElem] {
	val theArc=new Arc2D.Double
	
	def emptyFunc(ref:Reference)= new GraphElem(ref,0)
	
	registerClass(AllClasses.get.getClassIDByName("LineElem"),createLine)
	registerClass(AllClasses.get.getClassIDByName("ArcElem"),createArc)
	
	
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
	
	def createArc (ref:Reference,in:DataInput) = {
		val nfields=in.readByte
		if(nfields!=7) println("wrong number of fields "+nfields)
		val color=Expression.readConstant(in)
		val lineWidth=Expression.readConstant(in)
		val lineStyle=Expression.readConstant(in)
		val centerPoint=Expression.readConstant(in).toVector
		val diameter=Expression.readConstant(in).toDouble
		val startA=Expression.readConstant(in).toDouble
		val endA=Expression.readConstant(in).toDouble
		val owners=InstanceData.readOwners(in)
		//print("owners:"+owners.size+" "+owners.mkString)
		//print(" hasChildren:"+)
		in.readBoolean
		new ArcElement(ref,color.toInt,lineWidth.toInt,lineStyle.toInt,centerPoint,diameter,startA,endA)		
	}
	
	//def scalarProduct(ax:Double,ay:Double,bx:Double,by:Double,px:Double) = ax*bx+ay*by
	
	def getLineDistance(ax:Double,ay:Double,bx:Double,by:Double,px:Double,py:Double):Double = {
		val rx=bx-ax
		val ry=by-ay
		val rlen=Math.sqrt(rx*rx+ry*ry)
		if(rlen==0) return Math.MAX_DOUBLE
		val rnx=rx/rlen
		val rny=ry/rlen		
		val scale=(px-ax)*rnx+(py-ay)*rny
		val fx=ax+scale*rnx
		val fy=ay+scale*rny
		val dx=px-fx
		val dy=py-fy
		Math.sqrt(dx*dx+dy*dy)
	}
	
	
	
}