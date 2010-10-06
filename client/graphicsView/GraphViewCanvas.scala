/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import scala.swing.Component
import java.awt.{Graphics2D,Color,Dimension}

/**
 * 
 */
class GraphViewCanvas(controller:GraphViewController) extends Component {
	
	
	
	background=Color.white
	opaque=true
	override def paintComponent(g:Graphics2D)= {
		g.setPaint(Color.white)
		g.fillRect(0,0,size.width,size.height)
		
		g.setPaint(Color.black)
		for(lay <-controller.layerModel.layerList)
			for(elem<-lay.elemList)
					elem.draw(g,controller.scaleModel)
	}
	
	
}