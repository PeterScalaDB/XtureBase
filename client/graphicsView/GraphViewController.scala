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

/**
 * 
 */
class GraphViewController {
	
	
  val scaleModel=new ScaleModel
  val selectModel=new ViewSelectModel(this)
  val layerModel=new LayerTableModel()
    
  private var _viewportState=ViewportState.SelectState
  
  def viewportState=_viewportState  
	
	
		
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
		changeViewportState(ViewportState.ZoomInState )
	}
	
	def dragCompleted(startPoint:Point,endPoint:Point) = {
		_viewportState match {
			case ViewportState.ZoomInState => {
				scalePanel.zoomInBut.selected=false
				_viewportState=ViewportState.SelectState
				scaleModel.zoomIn(startPoint,endPoint)				
			}
			case _ =>
		}
	}
	
	def dragStopped() {
		_viewportState match {
			case ViewportState.ZoomInState => {
				scalePanel.zoomInBut.selected=false
				_viewportState=ViewportState.SelectState
			}
			case _ =>
		}		
	}
	
	def singleClick(where:Point) = {
		// check for single element selection
		val clickPosX=scaleModel.xToWorld(where.x)
		val clickPosY=scaleModel.yToWorld(where.y)
		val lineCatchDistance=canvas.lineCatchDistance.toDouble/scaleModel.scale
		println("Hittest clx:"+clickPosX+" cly:"+clickPosY+" dist:"+lineCatchDistance+" scale:"+scaleModel.scale)
		val hittedElements=filterLayersSelection(_.hits(clickPosX,clickPosY,lineCatchDistance) )
		println("hitted elements:"+hittedElements)
		if(hittedElements.isEmpty) selectModel.deselect(true)
		else selectModel.addSelection(hittedElements)
		canvas.repaint
	}
	
	def changeViewportState(newState:ViewportState.Value) = {
	  
		_viewportState=newState
	  
	  canvas.repaint
	}
	
	def stopModus() = {
		_viewportState match {
	  	case ViewportState.ZoomInState =>scalePanel.zoomInBut.selected=false
	  	case _ =>
	  }
		changeViewportState(ViewportState.SelectState)
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
				case Key.Escape => stopModus()
				case _ => //println(e)
		  }
			case o => // println(o)
		}
		
	}
	
	
}