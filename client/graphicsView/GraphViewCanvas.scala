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
	
	var pointHitPos:MatchingScreenPoints=null
	
	
	val selectColor=new Color(255,50,50)
	
	val dragTreshold=8 // how much pixels can you drag the mouse before it is handled as drag
	
	val lineCatchDistance=4
	val pointCatchDistance=5
	
	var lockedColor=new Color(0,70,50)
	
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
			val middleButton=((e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON2_DOWN_MASK)>0)
			val rightButton=((e.peer.getModifiersEx & java.awt.event.InputEvent.BUTTON3_DOWN_MASK)>0)
			val control=(e.modifiers & Key.Modifier.Control)>0			
			//println("inDistance :"+inDistance(dragStartPoint,e.point,dragTreshold)+" c:"+control+" middle:"+
			//	middleButton(e)+" "+controller.isZoomingIn)
			if(!inDistance(dragStartPoint,e.point,dragTreshold)&&middleButton&&control&&(!controller.isZoomingIn)){
				//println("zoom in")
				controller.isZoomingIn=true				
			}
			  
			if(dragToPoint!=null) drawDragGraphics()
			else repaint
			dragToPoint=e.point
			drawDragGraphics()
			
		}		
		
		case e:MouseReleased => {
			val control=(e.modifiers & Key.Modifier.Control)>0
			val shift=(e.modifiers & Key.Modifier.Shift)>0
			//println("released "+middleButton(e)+" "+(e.peer.getButton&java.awt.event.MouseEvent.BUTTON2>0))
			val middleButton=(e.peer.getButton==java.awt.event.MouseEvent.BUTTON2)
			val rightButton=(e.peer.getButton==java.awt.event.MouseEvent.BUTTON3)
			if(dragToPoint!=null&& !inDistance(dragStartPoint,dragToPoint,dragTreshold))
			{ // it was dragged
				controller.dragCompleted(dragStartPoint,dragToPoint,control,shift,rightButton)
				dragStartPoint=null
			} else { // it was NOT dragged
				controller.singleClick(dragStartPoint,control,shift,rightButton,middleButton)
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
				case ViewportState.AskPoint | ViewportState.LineTo => pointHitPos=controller.checkPointHit(e.point)
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
			case ViewportState.AskPoint => drawHitPoints(g)
			case ViewportState.LineTo => {
				drawHitPoints(g)
				g.setPaint(Color.black)
				if(!controller.lineToPointBuffer .isEmpty) {
					val lp=controller.lineToPointBuffer.last
					g.drawLine(currentMousePos.x,currentMousePos.y,controller.scaleModel.xToScreen(lp.x),
						controller.scaleModel.yToScreen(lp.y))
				}
			}
			case _ =>
		}
		g.setPaintMode()
	}
	
	private def drawHitPoints(g:Graphics2D)= {
		g.setPaint(Color.blue)
		if(pointHitPos.hitBoth.isDefined) g.drawRect(pointHitPos.hitBoth.get.x-4,pointHitPos.hitBoth.get.y-4,8,8)
		else {
			if(pointHitPos.hitX.isDefined){
				g.drawRect(pointHitPos.hitX.get.x-4,pointHitPos.hitX.get.y-4,8,8)
				g.drawLine(pointHitPos.hitX.get.x,pointHitPos.hitX.get.y,pointHitPos.hitX.get.x,currentMousePos.y)
			}
			if(pointHitPos.hitY.isDefined) {
				g.drawRect(pointHitPos.hitY.get.x-4,pointHitPos.hitY.get.y-4,8,8)	
				g.drawLine(pointHitPos.hitY.get.x,pointHitPos.hitY.get.y,currentMousePos.x,pointHitPos.hitY.get.y)
			}
		}
	}
	
	// is called by the controller to state that the layerList is empty
	def setHasVisibleLayers(visibleValue:Boolean) = {
		//peer.putClientProperty("newPanel",if(visibleValue) controller.newPanel else null)
	}
	
	override def paintComponent(g:Graphics2D)= {		
		g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
		val currBounds=if(g.getClipBounds==null)bounds else g.getClipBounds
		g.setPaint(Color.white)
		g.fillRect(0,0,size.width,size.height)
		
		if(hasFocus) {
		  g.setPaint(Color.blue)
		  g.setStroke(defaultStroke)
		  g.drawRect(0,0,size.width-1,size.height-1)
		}
		
		g.setPaint(Color.black)
		
		controller.viewportState match {
			case ViewportState.AskPoint => g.drawString("AskPoint",30,30) 
			case ViewportState.LineTo => g.drawString("LineTo",30,30)
			case _ =>
		}	
		
		// draw all elements
		for(lay <-controller.layerModel.layerList) {
			val lColor=if(lay.edible)null else lockedColor
			for(elem<-lay.elemList)
					elem.draw(g,controller.scaleModel,lColor)
		}
			
    // draw selected elements
		g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF ))			
		for(el <-controller.selectModel.list)
			el.draw(g,controller.scaleModel,selectColor)
		// draw Line-To graphics
		g.setStroke(defaultStroke)
		if(controller.viewportState == ViewportState.LineTo&& controller.lineToPointBuffer.size>1)
			for(i <- 0 until controller.lineToPointBuffer.size-1) {
				val sp=controller.lineToPointBuffer(i)
				val ep=controller.lineToPointBuffer(i+1)
				g.drawLine(controller.scaleModel.xToScreen(sp.x),controller.scaleModel.yToScreen(sp.y),
					controller.scaleModel.xToScreen(ep.x),controller.scaleModel.yToScreen(ep.y))
			}
		// draw braket cross
		if(controller.bracketMode) {
		  g.setStroke(defaultStroke)
		  g.setPaint(Color.black)
		  g.setXORMode(Color.yellow)
		  val brx=controller.scaleModel.xToScreen(controller.lastSelectedPoint.x)
		  val bry=controller.scaleModel.yToScreen(controller.lastSelectedPoint.y)
		  g.drawLine(brx-7,bry,brx+7,bry)
		  g.drawLine(brx,bry-7,brx,bry+7)
		  g.setPaintMode()
		}		
		
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
			case ViewportState.AskPoint | ViewportState.LineTo => {
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