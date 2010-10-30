/**
 * Author: Peter Started:03.10.2010
 */
package client.graphicsView

import java.awt._
import java.awt.geom._
import java.awt.BasicStroke
import definition.expression.VectorConstant

/** Manages the scale of a graphics view
 * 
 */
class ScaleModel {
	
	
	
	val vpBorder=10 // border of the Canvas
	
	private var _viewSize:Dimension=new Dimension(1,1) // size of the ViewPort component
	
	private var zoomStack=collection.immutable.List[Rectangle2D.Double]()
	
	private var _world_X:Double=_ // pos of the ViewPort in real world dimensions
	private var _world_Y:Double=_ 
	private var _world_Width:Double=1 // size of the ViewPort in real world dimensions
	private var _world_Height:Double=1	
	private var _heightSet:Boolean=_ // is the world height or width relevant for scaling 
	private var xOffset=0 // screen offset to center the drawing
	private var yOffset=0
	// world border
	private var wbx1:Double=0
	private var wby1:Double=0
	private var wbx2:Double=0
	private var wby2:Double=0
	
	
	private var _relativeScale=(1d,100d)
	
  private var _thicknessScale=1d
  def thicknessScale = _thicknessScale
	
	private var _dotPitch:Double=0.25 // display resolution in mm/pix
	
	private val scaleListeners=collection.mutable.HashSet[ ()=>Unit ]()
	
	val strokeMap=collection.mutable.HashMap[Int,BasicStroke]()
	
	def getStroke(thick:Int)= if(strokeMap.contains(thick)) strokeMap(thick)
													 else {
														 val newStroke=new BasicStroke(thicknessToScreen(thick).toFloat,BasicStroke.CAP_ROUND,
															 BasicStroke.JOIN_MITER)
														 //println("Stroke "+thick+" ->"+newStroke.getLineWidth)
														 strokeMap(thick)=newStroke
														 newStroke
													 }
	
	
	def viewSize=_viewSize
	def viewSize_=(newValue:Dimension) ={
		if(newValue.width-vpBorder*2!= _viewSize.width ||
			 newValue.height-vpBorder*2!= _viewSize.height	)
			_viewSize=new Dimension(newValue.width-vpBorder*2,newValue.height-vpBorder*2)
		notifyScaleChanged()	
		calcOffsets
	}
	
	def dotPitch=_dotPitch
	def dotPitch_= (newValue:Double)= {
		_dotPitch=newValue
	}
	
	def setWorldBounds(x:Double,y:Double,w:Double,h:Double) = {
		_world_X=x
		_world_Y=y
		_world_Width=if(w==0) 0.1 else w
		_world_Height=if(h==0) 0.1 else h		
		zoomStack=collection.immutable.List(new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height))
		calcOffsets		
		notifyScaleChanged()
	}
	
	def zoomIn(start:Point,end:Point) = {
		//println("zoom start:"+start+" end:"+end)
		//println("before zoom: world x:"+_world_X+" y:"+_world_Y+" w:"+_world_Width+" h:"+_world_Height+
		//	" xof:"+xOffset+" yof:"+yOffset+ " scale:"+scale)
		val x1=xToWorld(start.x)
		val x2=xToWorld(end.x)		
		val y1=yToWorld(start.y)
		//println("startY:"+start.y+" to world:"+y1+" back:"+yToScreen(y1))		
		val y2=yToWorld(end.y)
		//println("endY:"+end.y+" to world:"+y2+" back:"+yToScreen(y2))
		_world_X=Math.min(x1,x2)
		_world_Width=Math.max(x1,x2)-world_X		
		_world_Y=Math.min(y1,y2)
		_world_Height=Math.max(y1,y2)-world_Y
		zoomStack=new Rectangle2D.Double(_world_X,_world_Y,_world_Width,_world_Height):: zoomStack
		calcOffsets
		//println("after zoom: world x:"+_world_X+" y:"+_world_Y+" w:"+_world_Width+" h:"+_world_Height+
		//	" xof:"+xOffset+" yof:"+yOffset)
		
		notifyScaleChanged()
	}
	
	def zoomOut () = {
		//println("zoomout" + zoomStack)
		if(zoomStack.tail!=Nil){ 
			zoomStack=zoomStack.tail
			_world_X=zoomStack.head.x
			_world_Y=zoomStack.head.y
			_world_Width=zoomStack.head.width
			_world_Height=zoomStack.head.height
			calcOffsets
			notifyScaleChanged()
		}
	}
	
	def moveLeft= {
		_world_X-= _world_Width/4
		notifyScaleChanged()
	}
	
	def moveRight= {
		_world_X+= _world_Width/4
		notifyScaleChanged()
	}
	
	def moveUp= {
		_world_Y+= _world_Height/4
		notifyScaleChanged()
	}
	
	def moveDown= {
		_world_Y-= _world_Height/4
		notifyScaleChanged()
	}
	
	
	private def calcOffsets={
		val worldRatio= _world_Width*10d/_world_Height
		val viewRatio= if(_viewSize.height==0)viewSize.width*10/0.1 else _viewSize.width*10d/_viewSize.height
		_heightSet=worldRatio<viewRatio
		if(_heightSet){			
			yOffset=vpBorder
			xOffset=((_viewSize.width-_world_Width*scale)/2).toInt+vpBorder
			val worldOffset=(viewSize.width.toDouble/scale-_world_Width)/2
			wbx1=_world_X-worldOffset
			wby1= _world_Y	
			wbx2=_world_X+_world_Width+worldOffset
			wby2= _world_Y+_world_Height
		} else {
			xOffset=vpBorder
			yOffset=((_viewSize.height-_world_Height*scale)/2).toInt+vpBorder
			val worldOffset=(viewSize.height.toDouble/scale-_world_Height)/2
			wbx1=_world_X
			wby1=_world_Y-worldOffset	
			wbx2=_world_X+_world_Width
			wby2= _world_Y+_world_Height+worldOffset
		}
		_thicknessScale=(scale*_dotPitch/1000d)/(_relativeScale._1/_relativeScale._2)
		strokeMap.clear
		//println("thicknessScale:"+_thicknessScale)
		//println("calcOff world x:"+_world_X+" y:"+_world_Y+" w:"+_world_Width+" h:"+_world_Height+" view:"+_viewSize+
		//	" xof:"+xOffset+" yof:"+yOffset+" wr:"+worldRatio+" vr:"+viewRatio+" hs:"+_heightSet)
	}
	
	def world_X=_world_X
	
	def world_Y=_world_Y	
	
	def viewWidthInWorld=_world_Width
			
	def viewHeightInWorld=_world_Height
		
	def scale= {		
		if(_heightSet&&(_world_Height!=0)) {
			_viewSize.height.toDouble/_world_Height
		}else {
		  if(_world_Width==0) 1
		else(_viewSize.width.toDouble/_world_Width)	
		}
		//println("scale heightSet:"+_heightSet+" wh:"+_world_Height+" ww:"+_world_Width+ " sc:"+scal)
		//scal
	}
	
	def worldScale = scale*dotPitch
	
	def getScaleRatio:(Number,Number) = {
		val ret=scale*_dotPitch/1000d
	  if(ret>1) (ret,1) else (1,1/(ret))	
	}
	
	def setScaleRatio(a:Double,b:Double) = {
		
		val wishScale= (a / b)*1000d/_dotPitch
		println("SetScale "+a+" / "+b+" wsc:"+wishScale)
		var newWorldHeight:Double= 0
		var newWorldWidth:Double= 0
		if(_heightSet) {
			newWorldHeight=_viewSize.height.toDouble/wishScale
			newWorldWidth=_viewSize.width.toDouble*newWorldHeight/_viewSize.height.toDouble			
			
		} else {			
			newWorldWidth=_viewSize.width.toDouble/wishScale
			newWorldHeight=_viewSize.height.toDouble*newWorldWidth/_viewSize.width.toDouble			
		}
		val newX=_world_X-(newWorldHeight-_world_Height)/2
		val newY=_world_Y-(newWorldWidth-_world_Width)/2
		setWorldBounds(newX,newY,newWorldWidth,newWorldHeight)
	}
	
	
	def getWorldPos(screenPos:Point): (Double,Double) = {
		(0,0)
	}
	
	def xToScreen(wx:Double) = ((wx-_world_X)*scale).toInt+xOffset
	def yToScreen(wy:Double) = ((_world_Y+_world_Height-wy)*scale).toInt+yOffset
	
	def xToWorld(x:Int) = (x-xOffset).toDouble/scale+_world_X
	def yToWorld(y:Int) = (_world_Y+_world_Height)-((y-yOffset).toDouble/scale)
	
	def thicknessToScreen(thick:Double)=thick/(_dotPitch*100d)*_thicknessScale
	
	def getScreenPos(px:Double,py:Double):Point= new Point(xToScreen(px),yToScreen(py))	
	
	def registerScaleListener(listener:()=>Unit)= {
		scaleListeners+=listener
	}
	
	def notifyScaleChanged() = {
		for(l <-scaleListeners) l()
	}
	
	def relativeScale= _relativeScale
	def relativeScale_=(newScale:(Double,Double))= {
		_relativeScale=newScale
		calcOffsets
		notifyScaleChanged()
	}
	
	/** tests if the given Point is inside of the world bounds of the screen
	 * 
	 * @param tp the test point
	 * @return true if it is inside of the world bounds
	 */
	def isInWorldBounds(tp:VectorConstant)= tp.x>=wbx1 && tp.x<=wbx2 && tp.y>=wby1 && tp.y<=wby2
}

object ScalaModel {
	def main(args:Array[String]):Unit = {
		val sm=new ScaleModel()
		sm.viewSize=new Dimension(2200,1200)
		
		println(sm.scale+" "+sm.worldScale+" "+sm.getScaleRatio)
	}
}

