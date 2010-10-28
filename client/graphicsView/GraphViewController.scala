/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import scala.swing._
import scala.swing.event._
import definition.data._
import client.comm.ClientQueryManager
import javax.swing.SwingUtilities
import java.awt.event._
import client.dialog._
import definition.expression.VectorConstant

/**
 * 
 */
class GraphViewController {
	
	val newPanel=new GraphicsNewPanel
	
  val scaleModel=new ScaleModel
  val selectModel=new ViewSelectModel(this)
  val layerModel=new LayerTableModel()
	
	var pointListener:PointClickListener=_
	var lastSelectedPoint:VectorConstant=_
  
  // cache the state if the visible layer list is empty
  var cacheVisibleLayers=false
    
  private var _viewportState=ViewportState.SelectState
  
  def viewportState=_viewportState  
	
  var isZoomingIn=false
	
		
	val canvas=new GraphViewCanvas(this)
	val scalePanel=new ScalePanel(scaleModel,this)
	
  scaleModel.registerScaleListener(()=>{
  	canvas.repaint()
  })
	
	val canvasPanel=new BorderPanel {
		add(canvas,BorderPanel.Position.Center)
		add(scalePanel,BorderPanel.Position.North)
	}
	
	def filterLayersSelection(filtFunc:(GraphElem)=>Boolean):Seq[GraphElem]= {
		layerModel.layerList.flatMap(l => l.filterSelection(filtFunc))
	}
	
	canvas.peer.addHierarchyBoundsListener( new HierarchyBoundsListener(){			
			def ancestorMoved(e:HierarchyEvent ) ={}
			
			def ancestorResized(e:HierarchyEvent )= {
				 //if(e.getChanged==canvas.peer)
					scaleModel.viewSize=canvas.size			
			}
	})
	
	
	def layerChanged(lay:Layer) = {
		val hasVisibleLayers=layerModel.hasVisibleLayers
		if(hasVisibleLayers!=cacheVisibleLayers) {
			canvas.setHasVisibleLayers(hasVisibleLayers)
			cacheVisibleLayers=hasVisibleLayers
		}
		zoomAll
	}
	
	def graphElemAdded(lay:Layer,elem:GraphElem) = {
		canvas.repaint
	}
	
	def graphElemRemoved(lay:Layer,elem:GraphElem) = {
		selectModel.elemRemoved(elem)
		canvas.repaint
	}
	
	def graphElemChanged(lay:Layer,oldState:GraphElem,newState:GraphElem) = {
		selectModel.elemChanged(oldState,newState)
		canvas.repaint
	}
	// will be called when the DataViewController has another selection
	
		
	
	def zoomAll() = {
		val allBounds=layerModel.calcAllLayerBounds
		scaleModel.setWorldBounds(allBounds.x,allBounds.y,allBounds.width,allBounds.height)
		canvas.repaint
	}
	def zoomInClicked() = {
		isZoomingIn=true
	}
	
	def dragCompleted(startPoint:Point,endPoint:Point,control:Boolean,shift:Boolean) = {
		if(isZoomingIn){			
			scalePanel.zoomInBut.selected=false
			//_viewportState=ViewportState.SelectState
			scaleModel.zoomIn(startPoint,endPoint)				
			isZoomingIn=false
		} else
		_viewportState match {		
			case ViewportState.SelectState => {
				checkSelection(startPoint,endPoint,control)
			}
			case _ =>
		}
	}
	
	def dragStopped() {
		if(isZoomingIn) {
			scalePanel.zoomInBut.selected=false
			//_viewportState=ViewportState.SelectState
			isZoomingIn=false
		}
		_viewportState match {
			
				
			
			case _ =>
		}		
	}
	
	
	def checkSelection(startPoint:Point,endPoint:Point,add:Boolean) {
		val onlyInside=startPoint.x<endPoint.x // catch only Objects inside of the rectangle when 
		 // moving the mouse from left to right. When moving from right to left, catch all cutting objects
		val p1x=scaleModel.xToWorld(startPoint.x)
		val p2x=scaleModel.xToWorld(endPoint.x)
	  val minX=Math.min(p1x,p2x)
	  val maxX=Math.max(p1x,p2x)
	  val p1y=scaleModel.yToWorld(startPoint.y)
		val p2y=scaleModel.yToWorld(endPoint.y)
	  val minY=Math.min(p1y,p2y)
	  val maxY=Math.max(p1y,p2y)
	  val elemList= 
	  	if(onlyInside) filterLayersSelection(e=> e.minX>=minX && e.maxX<=maxX && e.minY>=minY && e.maxY<=maxY)
	  	else filterLayersSelection(e=> e.maxX>=minX && e.minX<=maxX && e.maxY>=minY && e.minY<=maxY)	  	
	  if(add) {
	  	if(!elemList.isEmpty)selectModel.addSelection(elemList,false)
	  } else {
	  	if (elemList.isEmpty)selectModel.deselect(true)
	  	else selectModel.setSelection(elemList)
	  }
	  
	}
	
	
	def singleClick(where:Point,control:Boolean,shift:Boolean) = {
		// check for single element selection
		val clickPosX=scaleModel.xToWorld(where.x)
		val clickPosY=scaleModel.yToWorld(where.y)
		
		//println("Hittest clx:"+clickPosX+" cly:"+clickPosY+" dist:"+lineCatchDistance+" scale:"+scaleModel.scale)
		
		_viewportState match {
			case ViewportState.SelectState => {
				val lineCatchDistance=canvas.lineCatchDistance.toDouble/scaleModel.scale
		    val hittedElements=filterLayersSelection(_.hits(clickPosX,clickPosY,lineCatchDistance) )
		    //println("hitted elements:"+hittedElements)
		    if(control) {
		    	if(!hittedElements.isEmpty) selectModel.addSelection(hittedElements,true)
		    }else 
		    	if(hittedElements.isEmpty) selectModel.deselect(true)
		    	else selectModel.setSelection(hittedElements) 		
			}
			case ViewportState.AskPoint => {
				getNearestPoint(clickPosX,clickPosY) match {
					case Some(nearestPoint) => pointListener.pointClicked(nearestPoint)
					case _ => pointListener.pointClicked(new VectorConstant(clickPosX,clickPosY,0))
				}			 				
			}
		}		
		canvas.repaint
	}
	
	def changeViewportState(newState:ViewportState.Value) = {
		stopModus
		_viewportState=newState	  
	  canvas.repaint
	}
	
	def stopModus() = {
		if(isZoomingIn){
		  scalePanel.zoomInBut.selected=false	
		}else 		
		_viewportState match {
	  	
	  	case _ =>
	  }		
	}
	
	def cancelModus() = {		
		changeViewportState(ViewportState.SelectState)
		canvas.repaint
	}
	
	def keyPressed(e:KeyPressed) = {
		//println("key typed "+e)
		_viewportState match {
			case ViewportState.SelectState =>
			e.key match {
				case Key.Left => scaleModel.moveLeft
				case Key.Right => scaleModel.moveRight
				case Key.Up => scaleModel.moveUp
				case Key.Down => scaleModel.moveDown
				case Key.Escape => cancelModus()
				case _ => //println(e)
		  }
			case ViewportState.AskPoint => 
			e.key match {				
				case Key.Escape => DialogManager.reset
				case _ => //println(e)
			}
			case o => // println(o)
		}		
	}
	
	def focusGained = PointAnswerPanel.currentViewController=this
	
	def askForPointClick(plistener:PointClickListener) = {
		pointListener=plistener
		changeViewportState(ViewportState.AskPoint)
	}
	
	def getNearestPoint(clickPosX:Double,clickPosY:Double):Option[VectorConstant] = {
		val pointCatchDistance=canvas.pointCatchDistance.toDouble/scaleModel.scale		
		val hittedPoints=layerModel.checkElementPoints(_.hitPoint(clickPosX, clickPosY, pointCatchDistance))
		if(! hittedPoints.isEmpty) {
			var nearestPoint:VectorConstant= null
			var dist:Double= Math.MAX_DOUBLE
			for(el <-hittedPoints) {
				val newDist=el.squareDistanceTo(clickPosX,clickPosY,0)
				if(newDist<dist) nearestPoint=el
			}
			Some(nearestPoint)
		}
		else None
	}
	
	/** in AskPoint modus: check if there is a fitting point next to the mouse cursor 
	 * 
	 * @param screenPos current screen pos of the mouse cursor
	 * @return screen pos of a hitting element point, or null if no points arround
	 */
	def checkPointHit(screenPos:Point):Point = {
		getNearestPoint(scaleModel.xToWorld(screenPos.x),scaleModel.yToWorld(screenPos.y)) match {
			case Some (nearestPoint) => new Point(scaleModel.xToScreen(nearestPoint.x),scaleModel.yToScreen(nearestPoint.y))
			case _ => null
		}
	  	
	}
	
}