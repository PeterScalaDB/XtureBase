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
  //val lineToPointBuffer=collection.mutable.ArrayBuffer[VectorConstant]()
  var rubberStartPoint:VectorConstant=null
	var currLineToFactory:(VectorConstant,VectorConstant)=>GraphElem=null		
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
		selectModel.elemRemoved(lay,elem)
		canvas.repaint
	}
	
	def graphElemChanged(lay:Layer,newState:GraphElem,repaint:Boolean=true) = {
		selectModel.elemChanged(lay,newState)
		if(repaint)canvas.repaint
	}
	
	
	def graphElementsChanged(lay:Layer,newStates:Seq[GraphElem],repaint:Boolean=true) = {
		selectModel.elementsChanged(lay,newStates)
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
	
	/** finds the elements that are in the given rectangle and selects them 
	 * in the select model
	 * 
	 * @param startPoint
	 * @param endPoint
	 * @param add
	 */
	def checkSelection(startPoint:Point,endPoint:Point,add:Boolean):Unit= {
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
	  	if(onlyInside) layerModel.filterLayersSelection(e=>{
	  		val eb=e.getBounds
	  		eb.x>=minX && eb.width<=maxX && eb.y>=minY && eb.height<=maxY
	  	})
	  	else layerModel.filterLayersSelection(e=>{
	  		val eb=e.getBounds
	  		eb.width>=minX && eb.x<=maxX && eb.height>=minY && eb.y<=maxY	  	
	  	})
	  System.out.println("check selection result="+elemList)
	  if(add) {
	  	for(it<-elemList)
	  	  selectModel.addSelection(it._1,it._2,false)
	  } else {
	  	if (elemList.isEmpty)selectModel.deselect(true)
	  	else for(it<-elemList)	  		
	  		selectModel.setSelection(it._1,it._2)
	  }
	  
	}
	
	
	def singleClick(where:Point,control:Boolean,shift:Boolean,rightButton:Boolean,middleButton:Boolean) = {
		// check for single element selection
		val clickPosX=scaleModel.xToWorld(where.x)
		val clickPosY=scaleModel.yToWorld(where.y)
		System.out.println("single Click "+rightButton+" "+middleButton)
		//System.out.println("Hittest clx:"+clickPosX+" cly:"+clickPosY+" dist:"+lineCatchDistance+" scale:"+scaleModel.scale)
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
		    val hittedElements=layerModel.filterLayersSelection(_.hits(clickPosX,clickPosY,lineCatchDistance) )
		    //System.out.println("hitted elements:"+hittedElements)
		    if(control) {
		    	for(el<-hittedElements)
		    	 selectModel.addSelection(el._1,el._2,true)
		    }else 
		    	if(hittedElements.isEmpty) selectModel.deselect(true)
		    	else for(el<-hittedElements)
		    		selectModel.setSelection(el._1,el._2) 		
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
						System.out.println("project both")
						new VectorConstant(nearestX.x,nearestY.y,0)						
					}
					case MatchingPoints(None,Some(nearestX),None) if(middleButton)=>{
						System.out.println("project x")
						new VectorConstant(nearestX.x,clickPosY,0)
					}
					case MatchingPoints(None,None,Some(nearestY)) if(middleButton)=> {
						System.out.println("project y")
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
			//lineToPointBuffer(lineToPointBuffer.size-1)=point
		}
		else {
			//lineToPointBuffer+=point
			processPoint(point)
		}
	}
	
	def processPoint(point:VectorConstant) = {
		_viewportState match {
			case ViewportState.LineTo => {
				val newElem=currLineToFactory(rubberStartPoint,point)
				layerModel.newElemLayer.addTempElement(newElem)
			}
			case _ =>
		}
		//System.out.println("processpoint rubber:"+rubberStartPoint+" point:"+point)
		rubberStartPoint=point
		pointListener.pointClicked(point)		
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
		layerModel.newElemLayer.shutDown
		changeViewportState(ViewportState.SelectState)		
		//canvas.repaint
	}
	
	def keyPressed(e:KeyPressed) = {
		//System.out.println("key typed "+e)
		_viewportState match {
			case ViewportState.SelectState =>
			e.key match {
				case Key.Left => scaleModel.moveLeft
				case Key.Right => scaleModel.moveRight
				case Key.Up => scaleModel.moveUp
				case Key.Down => scaleModel.moveDown
				case Key.Escape => cancelModus()
				case _ => //System.out.println(e)
		  }
			case ViewportState.AskPoint |  ViewportState.LineTo => 
			e.key match {				
				case Key.Escape => DialogManager.reset
				case _ => //System.out.println(e)
			}
			case o => // System.out.println(o)
		}		
	}
	
	def focusGained = {
		PointAnswerPanel.currentViewController=this
		notifyContainerListeners
	}
	
	def askForPointClick(plistener:PointClickListener) = {
		pointListener=plistener
		layerModel.newElemLayer.shutDown
		//lineToPointBuffer.clear
		changeViewportState(ViewportState.AskPoint)
	}
	
	def askForLineTo(plistener:PointClickListener,
	                 lineToFactory:(VectorConstant,VectorConstant)=>GraphElem) = {
		currLineToFactory=lineToFactory
		pointListener=plistener		
		changeViewportState(ViewportState.LineTo)
		System.out.println("askforLineTo")
	}
	
	def getNearestPoint(clickPosX:Double,clickPosY:Double):MatchingPoints = {
		val pointCatchDistance=canvas.pointCatchDistance.toDouble/scaleModel.scale		
		
		val rubberList = if(rubberStartPoint!=null) 
			GraphElemFactory.checkHit(clickPosX,clickPosY,pointCatchDistance,rubberStartPoint)
			else Seq.empty
		val hittedPoints=if(rubberList.isEmpty)
			layerModel.checkElementPoints(_.hitPoint(clickPosX, clickPosY, pointCatchDistance))
			else layerModel.checkElementPoints(_.hitPoint(clickPosX, clickPosY, pointCatchDistance))++rubberList	
		
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
		//lineToPointBuffer+=lastSelectedPoint
		canvas.repaint
	}
	
	def stopBracketMode() = {
		bracketMode=false	
		processPoint(lastSelectedPoint)
		canvas.repaint
	}
	
	def addDelta(dx:Double,dy:Double) = {
		internSetPoint(lastSelectedPoint +(dx,dy,0))
	}
	
	def setCoordinate(dx:Double,dy:Double) = {
		internSetPoint(new VectorConstant(dx,dy,0))
	}
}