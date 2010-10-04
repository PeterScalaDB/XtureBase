/**
 * Author: Peter Started:03.10.2010
 */
package client.graphicsView

import java.awt._

/** Manages the scale of a graphics view
 * 
 */
class ScaleModel {
	private var _viewSize:Dimension=_ // size of the ViewPort component
	
	private var _world_X:Double=_ // pos of the ViewPort in real world dimensions
	private var _world_Y:Double=_ 
	private var _world_Width:Double=_ // size of the ViewPort in real world dimensions
	private var _world_Height:Double=_	
	private var _heightSet:Boolean=_ // was the world height or width set 
	
	private var _dotPitch:Double=0.25 // display resolution in mm/pix
	
	def viewSize=_viewSize
	def viewSize_=(newValue:Dimension) ={
		_viewSize=newValue
		if(_heightSet) {
			
		}
		else {
			
		}
	}
	
	def dotPitch=_dotPitch
	def dotPitch_= (newValue:Double)= {
		_dotPitch=newValue
	}
	
	
	def world_X=_world_X
	def world_X_=(newValue:Double):Unit = {
		
    _world_X=newValue 		
	}	
	def world_Y=_world_Y
	def world_Y_=(newValue:Double) = {		
    _world_Y=newValue 		
	}
	
	
	def viewWidthInWorld=_world_Width
	def viewWidthInWorld_=(newValue:Double) = {
		_heightSet=false
		_world_Width=newValue
	}
		
	def viewHeightInWorld=_world_Height
	def viewHeightInWorld_=(newValue:Double) = {
		_heightSet=true
		_world_Height=newValue
	}	
	
	def scale= {
		if(_heightSet&&(_world_Height!=0)) {
			_viewSize.height.toDouble/_world_Height
		}else {
		  if(_world_Width==0) 1
		else(_viewSize.width.toDouble/_world_Width)	
		}		
	}
	
	def worldScale = scale*dotPitch
	
	def getScaleRatio:(Double,Double) = {
		val ret=scale*_dotPitch
	  if(ret>1) (ret,1) else (1,1/(ret))	
	}
	
	def getWorldPos(screenPos:Point): (Double,Double) = {
		(0,0)
	}
	
	def getScreenPos(worldX:Double,worldY:Double):Point= {
		new Point(0,0)
	}
	
	
	

}

object ScalaModel {
	def main(args:Array[String]):Unit = {
		val sm=new ScaleModel()
		sm.viewSize=new Dimension(2200,1200)
		sm.world_X=100
		sm.viewWidthInWorld=0.250*1000
		sm.viewHeightInWorld=0.150*1000
		println(sm.scale+" "+sm.worldScale+" "+sm.getScaleRatio)
	}
}