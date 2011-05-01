/**
 * Author: Peter Started:23.04.2011
 */
package client.print

import scala.swing.Component
import definition.data.PageData
import definition.data.FormDescription
import javax.swing.JComponent
import java.awt.Dimension
import java.awt.Color
import javax.swing.BorderFactory
import java.awt.Graphics2D
import definition.data.RenderContext
import java.awt.print.PageFormat
import java.awt.RenderingHints
import java.awt.geom.Rectangle2D
import java.awt.Rectangle

/**
 * 
 */
class PageViewer(context:RenderContext) extends Component {
  private val nullSize=new Dimension(0,0)
	var pageNr:Int=_	
	var data:APageable=_
	
	var prefSize:Dimension=getPrefSize
  //def scale:Float=context.getScale.toFloat
  
  ///background=Color.white
  //opaque=true
  //border=BorderFactory.createLineBorder(Color.black)

	def setData(nd:APageable) = {
  	data=nd
  	updateSize()
  }	
	
	def updateSize()= {		
		prefSize=getPrefSize
		//println("update prefSize="+prefSize+" pw:"+data.pageWidth+" ph:"+data.pageHeight)
		revalidate
		repaint
	}
  
  override lazy val peer=new JComponent with SuperMixin with javax.swing.Scrollable {
  	override def getPreferredSize=prefSize 
  	override def getMaximumSize=prefSize
  	override def getMinimumSize=prefSize
  	def getPreferredScrollableViewportSize: Dimension=getPreferredSize
  	def getScrollableTracksViewportHeight: Boolean =false
  	def getScrollableTracksViewportWidth: Boolean=false
  	def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
  	def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10
  }
  
  override def paintComponent(g:Graphics2D)= {
  	super.paintComponent(g)  	
  	g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
  	g.setColor(Color.white)
  	g.fillRect(0,0,prefSize.width,prefSize.height)
  	g.setColor(Color.black)
  	g.drawRect(0,0,prefSize.width-1,prefSize.height-1)
  	g.setColor(Color.blue)
  	g.draw(data.clipRect)
  	
  	//g.drawRect(0,0,fromMM(pageWidth), fromMM(pageHeight))
  	g.setColor(Color.black)  	
  	data.print(g, null,pageNr-1)  	
  }
  
  private def fromMM(value:Float):Int = {
  	context.toUnit(value).toInt
  	//form.fromMM(value*scale).toInt
  }
	  
  
  
	def getPrefSize= if(data==null)nullSize 
	  else new Dimension(fromMM(data.pageWidth.toFloat),fromMM(data.pageHeight.toFloat))
}