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
abstract class GraphElem(override val ref:Reference,val color:Int) extends Referencable {
  def getBounds:Rectangle2D.Double // width counts as maxX, height as maxY	
  def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)
  /*def minX:Double
  def maxX:Double
  def minY:Double
  def maxY:Double*/
  def hits(px:Double,py:Double,dist:Double):Boolean // hittest  
  def hitPoint(px:Double,py:Double,dist:Double):Seq[(Byte,VectorConstant)]  
}

object ColorMap {
	val theMap=collection.mutable.HashMap[Int,Color]()
	def getColor(col:Int)= 
		if(theMap.contains(col)) theMap(col)
		else {
			val newCol=new Color(col)
			theMap(col)=newCol
			newCol
		}
}

abstract class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor)

case class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,startPoint:VectorConstant,endPoint:VectorConstant) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=new Rectangle2D.Double(Math.min(startPoint.x,endPoint.x),Math.min(startPoint.y,endPoint.y),
		Math.max(startPoint.x,endPoint.x),Math.max(startPoint.y,endPoint.y))
	override def getBounds=bounds
	override def toString= "Line ("+startPoint.shortToString+","+endPoint.shortToString+", Col:"+color+", Style:"+lineStyle+")"
	
	override def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)={
		g.setPaint(if(selectColor==null) ColorMap.getColor(color)else selectColor)		
		g.setStroke(sm.getStroke(if(lineWidth>0)lineWidth else 1))
		g.drawLine(sm.xToScreen(startPoint.x) ,sm.yToScreen(startPoint.y) ,
			sm.xToScreen(endPoint.x),sm.yToScreen(endPoint.y))	
	}
	def minX=if(startPoint.x<endPoint.x) startPoint.x else endPoint.x
	def maxX=if(startPoint.x>endPoint.x) startPoint.x else endPoint.x
	def minY=if(startPoint.y<endPoint.y) startPoint.y else endPoint.y
	def maxY=if(startPoint.y>endPoint.y) startPoint.y else endPoint.y
	
	override def hits(px:Double,py:Double,dist:Double)= {
		val calcDist=GraphElemFactory.getLineDistance(startPoint.x,startPoint.y,endPoint.x,endPoint.y,px,py)
		//println("hittest startp:"+startPoint+" dist:"+calcDist)
		calcDist<=dist && px>=(minX - dist) && (px<=(maxX)+dist) &&
		  py>=(minY-dist) && (py<=(maxY)+dist)
	}
	override def hitPoint(px:Double,py:Double,dist:Double)= {		
		//println("test x:"+(px-startPoint.x)+ " y:"+(py-startPoint.y))
		val ret1=GraphElemFactory.checkHit(px,py,dist,startPoint)
		val ret2=GraphElemFactory.checkHit(px,py,dist,endPoint)
		if(ret1.isEmpty) {
			if (ret2.isEmpty) Nil
			else ret2
		} else {
			if(ret2.isEmpty) ret1
			else List(ret1.head,ret2.head)
		}		
	}
}


case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
	diameter:Double,startAngle:Double,endAngle:Double) extends 
		LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
	lazy val bounds=calcArcBounds
	lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle),pointFromAngle(endAngle),centerPoint)
	//var pointBuffer:collection.mutable.ArrayBuffer[VectorConstant]=null
	override def getBounds=bounds
	override def toString= "Arc ("+centerPoint.shortToString+") d="+diameter+", sa:"+startAngle+", eA:"+endAngle+")"
	
	override def draw(g:Graphics2D,sm:ScaleModel,selectColor:Color=null)={
		g.setPaint(if(selectColor==null)ColorMap.getColor(color)else selectColor)
		g.setStroke(sm.getStroke(if(lineWidth>0)lineWidth else 1))
		
		val tx=sm.xToScreen(centerPoint.x-diameter)
		val ty=sm.yToScreen(centerPoint.y+diameter)
		GraphElemFactory.theArc.setArc(tx.toDouble,ty.toDouble ,sm.xToScreen(centerPoint.x+diameter)-tx,
			sm.yToScreen(centerPoint.y-diameter)-ty,
			startAngle, ((if(endAngle<startAngle)360 else 0)+endAngle-startAngle),Arc2D.OPEN)
		g.draw(GraphElemFactory.theArc)
		val mx=sm.xToScreen(centerPoint.x)
		val my=sm.yToScreen(centerPoint.y)
		g.drawLine(mx,my,mx,my)		
		/*g.setPaint(Color.red)
		if(pointBuffer!=null)
		for(p <-pointBuffer) {
			val px=sm.xToScreen(p.x)
			val py=sm.yToScreen(p.y)
			g.drawLine(px,py,px,py)
		}*/
	}	
	
	override def hits(px:Double,py:Double,dist:Double):Boolean= {
		val dx=px-centerPoint.x
		val dy=py-centerPoint.y
		val pd=Math.sqrt(dx*dx+dy*dy)
		if(Math.abs(pd-diameter)>dist) return false
		var angle=Math.atan2(dy,dx)*180/Math.Pi
		if(angle<0) angle=360+angle
		//println("Hittest angle:"+angle+" sa:"+startAngle+" ea:"+endAngle)
		if(startAngle<endAngle) return (angle>=startAngle)&&(angle<=endAngle)
		else return (angle>=startAngle)||(angle<=endAngle)
	}
	
	override def hitPoint(px:Double,py:Double,dist:Double)= {		
		//println("test x:"+(px-startPoint.x)+ " y:"+(py-startPoint.y))	
		points.flatMap(GraphElemFactory.checkHit(px,py,dist,_))		
	}
	
	def calcArcBounds= {
		val pointBuffer=collection.mutable.ArrayBuffer[VectorConstant]()+=points.head+=points.tail.head
		val ea=if(endAngle<startAngle) endAngle+360 else endAngle
		var nextSegmentAngle=(Math.floor(startAngle/90)+1)*90
		//println("startAngle "+startAngle+" "+nextSegmentAngle+" ea:"+ea)
		while (nextSegmentAngle<ea) {
			val np=pointFromAngle(nextSegmentAngle)
			//println("nxa:"+nextSegmentAngle+"Np "+np)
			pointBuffer+=np
			nextSegmentAngle+=90
		}
		//println("calcArcBounds "+pointBuffer.mkString)
		val b=GraphElemFactory.getPointsBounds(pointBuffer)
		//println("bounds: "+b)
		
		b
	}
	def pointFromAngle(angle:Double) = 
		new VectorConstant(centerPoint.x+Math.cos(angle*Math.Pi/180d)*diameter,
			centerPoint.y+Math.sin(angle*Math.Pi/180d)*diameter,0)	
	
}





object GraphElemFactory extends SubscriptionFactory[GraphElem] {
	val theArc=new Arc2D.Double
	
	val HITX=1.toByte
	val HITY=2.toByte
	val HITBOTH=3.toByte
	
	def emptyFunc(ref:Reference)= new LineElement(ref,0,0,0,null,null)
	
	registerClass(AllClasses.get.getClassIDByName("LineElem"),createLine)
	registerClass(AllClasses.get.getClassIDByName("ArcElem"),createArc)
	
	
	def createLine (ref:Reference,in:DataInput) = {
		val nfields=in.readByte
		if(nfields!=5) println("wrong number of fields "+nfields+" "+ref)
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
		if(nfields!=7) println("wrong number of fields "+nfields+ " "+ref)
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
	
	
	// Service-Routines -------------------------------------------------
	
	
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
	
	def checkHit(px:Double,py:Double,dist:Double,p:VectorConstant):Seq[(Byte,VectorConstant)]={
  	val xHit=Math.abs(px-p.x)<dist
  	val yHit=Math.abs(py-p.y)<dist
  	if(xHit&&yHit) List((GraphElemFactory.HITBOTH,p))
  	else if(xHit)List((GraphElemFactory.HITX,p))
  	else if(yHit)List((GraphElemFactory.HITY,p))
  	else Nil
  }
	
	/** width counts as maxX, height as maxY !!!
	 * 
	 */
	def getPointsBounds(points:Seq[VectorConstant]):Rectangle2D.Double = 
		if(points==null && points.isEmpty) return null
		else 	{
			val result=new Rectangle2D.Double(points.head.x,points.head.y,points.head.x,points.head.y)
			if(points.size>1) for(ix <-1 until points.size) {
				val p=points(ix)
				if(p.x<result.x){ result.x=p.x }
				if(p.y<result.y){ result.y=p.y }
				if(p.x>result.width) result.width=p.x
				if(p.y>result.height) result.height=p.y
			}
			result
		}
	
	def rectToScreen(r:Rectangle2D.Double,sm:ScaleModel)= {
		val x=sm.xToScreen(r.x)
		val y=sm.yToScreen(r.y)
		val y2=sm.yToScreen(r.height+r.y)
		new Rectangle2D.Double(x,y2,sm.xToScreen(r.width+r.x)-x,y-y2)
	}
}