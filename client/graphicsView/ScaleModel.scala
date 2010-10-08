/**
 * Author: Peter Started:03.10.2010
 */
package client.graphicsView

import java.awt._

/** Manages the scale of a graphics view
 * 
 */
class ScaleModel {
	
	val vpBorder=10 // border of the Canvas
	
	private var _viewSize:Dimension=new Dimension(1,1) // size of the ViewPort component
	
	private var _world_X:Double=_ // pos of the ViewPort in real world dimensions
	private var _world_Y:Double=_ 
	private var _world_Width:Double=1 // size of the ViewPort in real world dimensions
	private var _world_Height:Double=1	
	private var _heightSet:Boolean=_ // is the world height or width relevant for scaling 
	private var xOffset=0 // screen offset to center the drawing
	private var yOffset=0
	
	private var _dotPitch:Double=0.25 // display resolution in mm/pix
	
	private val scaleListeners=collection.mutable.HashSet[ ()=>Unit ]()
	
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
		_world_Width=w
		_world_Height=if(h==0) 0.1 else h		
		calcOffsets		
		notifyScaleChanged()
	}
	
	private def calcOffsets={
		val worldRatio=if(_world_Height==0)_world_Width*10/0.1 else _world_Width*10d/_world_Height
		val viewRatio= if(_viewSize.height==0)viewSize.width*10/0.1 else _viewSize.width*10d/_viewSize.height
		_heightSet=worldRatio<viewRatio
		if(_heightSet){			
			yOffset=vpBorder
			xOffset=((_viewSize.width-_world_Width*scale)/2).toInt+vpBorder
		} else {
			xOffset=vpBorder
			yOffset=((_viewSize.height-_world_Height*scale)/2).toInt+vpBorder
		}
		//println("calcOff world x:"+_world_X+" y:"+_world_Y+" w:"+_world_Width+" h:"+_world_Height+" view:"+_viewSize+
		//	" xof:"+xOffset+" yof:"+yOffset+" wr:"+worldRatio+" vr:"+viewRatio+" hs:"+_heightSet)
	}
	
	def world_X=_world_X
	/*def world_X_=(newValue:Double):Unit = {		
    _world_X=newValue 		
    notifyScaleChanged()
	}	*/
	def world_Y=_world_Y
	/*def world_Y_=(newValue:Double) = {		
    _world_Y=newValue 	
    notifyScaleChanged()
	}*/
	
	
	def viewWidthInWorld=_world_Width
	/*def viewWidthInWorld_=(newValue:Double) = {
		_heightSet=false
		_world_Width=newValue
		notifyScaleChanged()
	}*/
		
	def viewHeightInWorld=_world_Height
	/*def viewHeightInWorld_=(newValue:Double) = {
		_heightSet=true
		_world_Height=newValue
		notifyScaleChanged()
	}	*/
	
	def scale= {
		if(_heightSet&&(_world_Height!=0)) {
			_viewSize.height.toDouble/_world_Height
		}else {
		  if(_world_Width==0) 1
		else(_viewSize.width.toDouble/_world_Width)	
		}		
	}
	
	def worldScale = scale*dotPitch
	
	def getScaleRatio:(Number,Number) = {
		val ret=scale*_dotPitch/1000d
	  if(ret>1) (ret,1) else (1,1/(ret))	
	}
	
	
	def getWorldPos(screenPos:Point): (Double,Double) = {
		(0,0)
	}
	
	def xToScreen(wx:Double) = ((wx-_world_X)*scale).toInt+xOffset
	def yToScreen(wy:Double) = ((_world_Y+_world_Height-wy)*scale).toInt+yOffset
	
	def getScreenPos(px:Double,py:Double):Point= new Point(xToScreen(px),yToScreen(py))	
	
	def registerScaleListener(listener:()=>Unit)= {
		scaleListeners+=listener
	}
	
	def notifyScaleChanged() = {
		for(l <-scaleListeners) l()
	}
}

object ScalaModel {
	def main(args:Array[String]):Unit = {
		val sm=new ScaleModel()
		sm.viewSize=new Dimension(2200,1200)
		
		println(sm.scale+" "+sm.worldScale+" "+sm.getScaleRatio)
	}
}