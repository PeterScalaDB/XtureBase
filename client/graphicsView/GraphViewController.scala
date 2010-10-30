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

case class MatchingPoints(hitBoth:Option[VectorConstant],hitX:Option[VectorConstant],hitY:Option[VectorConstant])

case class MatchingScreenPoints(hitBoth:Option[Point],hitX:Option[Point],hitY:Option[Point])

class GraphViewController {
	
	//val newPanel=new GraphicsNewPanel(this)
	
	val containerFocusListeners=collection.mutable.HashSet[ContainerFocusListener]()
	
  val scaleModel=new ScaleModel
  val selectModel=new ViewSelectModel(this)
  val layerModel=new LayerTableModel(this)
	
	var pointListener:PointClickListener=_
	var lastSelectedPoint:VectorConstant=new VectorConstant(0,0,0)
	
	var bracketMode:Boolean = false
  
  // cache the state if the visible layer list is empty
  var cacheVisibleLayers=false
    
  private var _viewportState=ViewportState.SelectState
  
  def viewportState=_viewportState  
	
  var isZoomingIn=false
  
  val lineToPointBuffer=collection.mutable.ArrayBuffer[VectorConstant]()
	
		
	val canvas=new GraphViewCanvas(this)
	val scalePanel=new ScalePanel(scaleModel,this)
	
  scaleModel.registerScaleListener(()=>{
  	canvas.repaint()
  })
	
	val canvasPanel=new BorderPanel {
		add(canvas,BorderPanel.Position.Center)
		add(scalePanel,BorderPanel.Position.North)
	}
	
	
	
	canvas.peer.addHierarchyBoundsListener( new HierarchyBoundsListener(){			
			def ancestorMoved(e:HierarchyEvent ) ={}
			
			def ancestorResized(e:HierarchyEvent )= {
				 //if(e.getChanged==canvas.peer)
					scaleModel.viewSize=canvas.size			
			}
	})
	
	def filterLayersSelection(filtFunc:(GraphElem)=>Boolean):Seq[GraphElem]= {
		layerModel.layerList.flatMap(l => l.filterSelection(filtFunc))
	}
	
	def registerContainerListener(newList:ContainerFocusListener) = 
		containerFocusListeners+=newList
		
	def notifyContainerListeners = {
		if(!layerModel.layerList .isEmpty)
		for(list<-containerFocusListeners)
			list.containerFocused(layerModel.getActiveLayer,0,"Graph2DEdit")
	}
	
	
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
	
	def graphElemChanged(newState:GraphElem,repaint:Boolean=true) = {
		selectModel.elemChanged(newState)
		if(repaint)canvas.repaint
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
	
	def dragCompleted(startPoint:Point,endPoint:Point,control:Boolean,shift:Boolean,rightButton:Boolean) = {
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
	
	
	def singleClick(where:Point,control:Boolean,shift:Boolean,rightButton:Boolean,middleButton:Boolean) = {
		// check for single element selection
		val clickPosX=scaleModel.xToWorld(where.x)
		val clickPosY=scaleModel.yToWorld(where.y)
		println("single Click "+rightButton+" "+middleButton)
		//println("Hittest clx:"+clickPosX+" cly:"+clickPosY+" dist:"+lineCatchDistance+" scale:"+scaleModel.scale)
		if(rightButton) {
			_viewportState match { // switch bracket mode
				case ViewportState.AskPoint| ViewportState.LineTo => {
					if(bracketMode) stopBracketMode
					else startBracketMode
				}
				case _ => 
			}
		} else _viewportState match {
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
			case ViewportState.AskPoint | ViewportState.LineTo => {					
				internSetPoint(findMatchingPoint(clickPosX,clickPosY,middleButton))
			}			
		}		
		canvas.repaint
	}
	
	private def findMatchingPoint(clickPosX:Double,clickPosY:Double,middleButton:Boolean) = {
		getNearestPoint(clickPosX,clickPosY) match {
					case MatchingPoints(Some(nearestPoint),_,_) => nearestPoint
					case MatchingPoints(None,Some(nearestX),Some(nearestY)) if(middleButton)=> {
						println("project both")
						new VectorConstant(nearestX.x,nearestY.y,0)						
					}
					case MatchingPoints(None,Some(nearestX),None) if(middleButton)=>{
						println("project x")
						new VectorConstant(nearestX.x,clickPosY,0)
					}
					case MatchingPoints(None,None,Some(nearestY)) if(middleButton)=> {
						println("project y")
						new VectorConstant(clickPosX,nearestY.y,0)
					}
					case _ => new VectorConstant(clickPosX,clickPosY,0)
				}							
	}
	
	def internSetPoint(point:VectorConstant) = {		
		lastSelectedPoint=point
		if(bracketMode) {
			canvas.repaint
			//if(lineToPointBuffer.isEmpty) lineToPointBuffer+=point
			lineToPointBuffer(lineToPointBuffer.size-1)=point
		}
		else {
			lineToPointBuffer+=point
			pointListener.pointClicked(point)
		}
	}
	
	def changeViewportState(newState:ViewportState.Value) = {
		stopModus
		_viewportState=newState	  
	  canvas.repaint
	}
	
	def stopModus() = {
		bracketMode=false
		if(isZoomingIn){
		  scalePanel.zoomInBut.selected=false	
		}else 		
		_viewportState match {
	  	
	  	case _ =>
	  }		
	}
	
	def cancelModus() = {		
		changeViewportState(ViewportState.SelectState)
		bracketMode=false
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
			case ViewportState.AskPoint |  ViewportState.LineTo => 
			e.key match {				
				case Key.Escape => DialogManager.reset
				case _ => //println(e)
			}
			case o => // println(o)
		}		
	}
	
	def focusGained = {
		PointAnswerPanel.currentViewController=this
		notifyContainerListeners
	}
	
	def askForPointClick(plistener:PointClickListener) = {
		pointListener=plistener
		lineToPointBuffer.clear
		changeViewportState(ViewportState.AskPoint)
	}
	
	def askForLineTo(plistener:PointClickListener) = {
		pointListener=plistener		
		changeViewportState(ViewportState.LineTo)
	}
	
	def getNearestPoint(clickPosX:Double,clickPosY:Double):MatchingPoints = {
		val pointCatchDistance=canvas.pointCatchDistance.toDouble/scaleModel.scale		
		val hittedPoints=layerModel.checkElementPoints(_.hitPoint(clickPosX, clickPosY, pointCatchDistance))
		if(! hittedPoints.isEmpty) {
			var nearestPoint:VectorConstant= null
			var nearestX:VectorConstant=null
			var nearestY:VectorConstant=null
			var dist:Double= Math.MAX_DOUBLE
			var Xdist:Double= Math.MAX_DOUBLE
			var Ydist:Double= Math.MAX_DOUBLE
			for(el <-hittedPoints)
				if(scaleModel.isInWorldBounds(el._2))
				el._1 match {
				case GraphElemFactory.HITBOTH =>
					val newDist=el._2.squareDistanceTo(clickPosX,clickPosY,0)
					if(newDist<dist)  {nearestPoint=el._2;dist=newDist}
				case GraphElemFactory.HITX =>
				  val newDist=Math.abs(el._2.x-clickPosX)
				  if(newDist<Xdist) {nearestX=el._2 ;Xdist=newDist}
				case GraphElemFactory.HITY =>
				  val newDist=Math.abs(el._2.y-clickPosY)
				  if(newDist<Ydist) {nearestY=el._2 ;Ydist=newDist}
			}
			MatchingPoints(if(nearestPoint==null) None else Some(nearestPoint),
			 if(nearestX==null) None else Some(nearestX),
			 if(nearestY==null) None else Some(nearestY))
		}
		else MatchingPoints(None,None,None)
	}
	
	/** in AskPoint modus: check if there is a fitting point next to the mouse cursor 
	 * 
	 * @param screenPos current screen pos of the mouse cursor
	 * @return screen pos of a hitting element point, or null if no points arround
	 */
	def checkPointHit(screenPos:Point):MatchingScreenPoints = {
		val worldPoints=getNearestPoint(scaleModel.xToWorld(screenPos.x),scaleModel.yToWorld(screenPos.y))
		MatchingScreenPoints(optionToScreen(worldPoints.hitBoth),optionToScreen(worldPoints.hitX),
			optionToScreen(worldPoints.hitY))
			  	
	}
	
	private def optionToScreen(worldPos:Option[VectorConstant]):Option[Point] = 
		if(worldPos.isDefined) Option(new Point(scaleModel.xToScreen(worldPos.get.x),scaleModel.yToScreen(worldPos.get.y)))
		else None
	
	def requestFocus() = {
		canvas.requestFocusInWindow
		canvas.repaint
	}
	
	def startBracketMode() = {
		bracketMode=true
		lineToPointBuffer+=lastSelectedPoint
		canvas.repaint
	}
	
	def stopBracketMode() = {
		bracketMode=false		
		pointListener.pointClicked(lastSelectedPoint)
		canvas.repaint
	}
	
	def addDelta(dx:Double,dy:Double) = {
		internSetPoint(lastSelectedPoint +(dx,dy,0))
	}
	
	def setCoordinate(dx:Double,dy:Double) = {
		internSetPoint(new VectorConstant(dx,dy,0))
	}
}