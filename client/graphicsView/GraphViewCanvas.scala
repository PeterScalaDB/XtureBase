/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import scala.swing.Component
import java.awt.{Graphics2D,Color,Dimension,Point,Cursor,Toolkit,Rectangle,BasicStroke,RenderingHints}
import scala.swing.event._

/**
 * 
 */
class GraphViewCanvas(controller:GraphViewController) extends Component  {
	var dragStartPoint:Point=null
	var dragToPoint:Point=null
	var currentMousePos:Point=null
	
	var pointHitPos:Point=null
	
	
	val selectColor=new Color(255,50,50)
	
	val dragTreshold=5 // how much pixels can you drag the mouse before it is handled as drag
	
	val lineCatchDistance=4
	val pointCatchDistance=5
	
	// insert new panel information into properties
	
	setHasVisibleLayers(false)
	
		
	background=Color.white
	opaque=true
	focusable=true
	
	var inside=true
	var drawCrossHairInPaint=false	
	
	val dotCurs=toolkit.createCustomCursor(toolkit.getImage("dot_clear.gif"), new Point(0,0), "Zero");
	cursor=dotCurs
	
	val defaultStroke=new BasicStroke()
	
	listenTo(mouse.clicks,mouse.moves,keys,this)
	
	reactions+={
		case e:MousePressed => {
			requestFocusInWindow()
			val middleButton=(e.peer.getButton == java.awt.event.MouseEvent.BUTTON2)
			//val middleButton=(e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
			
			if(middleButton) controller.isZoomingIn=true
			currentMousePos=null
			dragStartPoint=e.point
			dragToPoint=null
			cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
		}
		case e:MouseEntered => {	
			
			currentMousePos=e.point
			//drawCrossHair
		}
		
		case e:MouseDragged => {
			if(dragToPoint!=null) drawDragGraphics()
			else repaint
			dragToPoint=e.point
			drawDragGraphics()
			
		}		
		
		case e:MouseReleased => {
			val control=(e.modifiers & Key.Modifier.Control)>0
			val shift=(e.modifiers & Key.Modifier.Shift)>0
			if(dragToPoint!=null&& !inDistance(dragStartPoint,dragToPoint,dragTreshold))
			{ // it was dragged
				controller.dragCompleted(dragStartPoint,dragToPoint,control,shift)
				dragStartPoint=null
			} else { // it was NOT dragged
				controller.singleClick(dragStartPoint,control,shift)
			}
			cursor=dotCurs
			currentMousePos=e.point
			drawCrossHairInPaint=true
			repaint()			
		}
		
		case e:MouseMoved => {
			if(!inside) {
				inside=true
				//println("not iside "+e.point)
			}
			else if(currentMousePos!=null) drawCrossHair()
			currentMousePos=e.point
			controller.viewportState match {
				case ViewportState.AskPoint => pointHitPos=controller.checkPointHit(e.point)
				case _ =>
			}
			drawCrossHair()
		}		
		
		case e:MouseExited => {
			currentMousePos=null
			if(dragStartPoint!=null) {
				dragStartPoint==null
				controller.dragStopped
				cursor=dotCurs
			}
			repaint()
			inside=false
		}		
		
		case e:KeyPressed => controller.keyPressed(e)
		
		case e:FocusLost => repaint
		
		case e:FocusGained => controller.focusGained
	}
	
	private def inDistance(a:Point,b:Point,distance:Int) = 
		Math.abs(a.x-b.x)<distance && Math.abs(a.y-b.y)<distance
	
	
	def drawCrossHair(g:Graphics2D=peer.getGraphics.asInstanceOf[Graphics2D]) = {		
		var currBounds=bounds 
		g.setXORMode(Color.white)
		g.setPaint(Color.black)
		g.drawLine(currentMousePos.x,1,currentMousePos.x,currBounds.height)
		g.drawLine(1,currentMousePos.y,currBounds.width,currentMousePos.y)
		controller.viewportState match {
			case ViewportState.AskPoint => {
				if(pointHitPos!=null) g.drawRect(pointHitPos.x-4,pointHitPos.y-4,8,8)
			}
			case _ =>
		}
		g.setPaintMode()
	}
	
	// is called by the controller to state that the layerList is empty
	def setHasVisibleLayers(visibleValue:Boolean) = {
		peer.putClientProperty("newPanel",if(visibleValue) controller.newPanel else null)
	}
	
	override def paintComponent(g:Graphics2D)= {		
		g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
		val currBounds=if(g.getClipBounds==null)bounds else g.getClipBounds
		g.setPaint(Color.white)
		g.fillRect(0,0,size.width,size.height)
		
		if(hasFocus) {
		  g.setPaint(Color.red)
		  g.setStroke(defaultStroke)
		  g.drawRect(0,0,size.width-1,size.height-1)
		}
		
		g.setPaint(Color.black)
		
		controller.viewportState match {
			case ViewportState.AskPoint => g.drawString("AskPoint",30,30)  
			case _ =>
		}	
		
		// draw all elements
		for(lay <-controller.layerModel.layerList)
			for(elem<-lay.elemList)
					elem.draw(g,controller.scaleModel)
    // draw selected elements
		g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF ))			
		for(el <-controller.selectModel.list)
			el.draw(g,controller.scaleModel,selectColor)
		g.setStroke(defaultStroke)    
		if(dragStartPoint!=null)
			drawDragGraphics(g)
		if(drawCrossHairInPaint) {
			drawCrossHairInPaint=false
			//println("In paint")
			drawCrossHair(g)
		}
		
	}
	
	def drawDragGraphics(g:Graphics2D=peer.getGraphics.asInstanceOf[Graphics2D]) = {
		if(dragToPoint!=null)
		controller.viewportState match {
			case ViewportState.SelectState => {
				drawDragRect(g)
			}
			case ViewportState.AskPoint => {
				if(controller.isZoomingIn)
					drawDragRect(g)
			}
		}
	}
	
	def drawDragRect(g:Graphics2D)= {
		g.setXORMode(Color.white)
		g.setPaint(Color.gray)
		val sx=Math.min(dragStartPoint.x,dragToPoint.x)
		val sy=Math.min(dragStartPoint.y,dragToPoint.y)
		g.drawRect(sx,sy,Math.max(dragToPoint.x,dragStartPoint.x)-sx,
		Math.max(dragToPoint.y,dragStartPoint.y)-sy)
		g.setPaintMode()
	}
	
	
}