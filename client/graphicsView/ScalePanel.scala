/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import scala.swing._
import scala.swing.event._
import java.awt.{Font,Color,Insets}

/**
 * 
 */
class ScalePanel(model:ScaleModel,controller:GraphViewController) extends BoxPanel(scala.swing.Orientation.Horizontal) {
		
	val zoomAllBut=new Button("Zoom All")
	val zoomInBut=new ToggleButton(" + ")
	val zoomOutBut=new Button(" - ")
	val scaleEdit=new TextField("")
	val relativeEdit=new TextField("1 : 100")
	val smallFont=new Font("Arial",0,10)
	zoomAllBut.font=(smallFont)
	zoomInBut.font=smallFont
	zoomOutBut.font=smallFont
	val miniInsets=new Insets(0,5,0,5)
	zoomAllBut.margin=miniInsets
	zoomInBut.margin=miniInsets
	zoomOutBut.margin=miniInsets
	relativeEdit.maximumSize=new Dimension(70,30)
	relativeEdit.preferredSize=relativeEdit.maximumSize
	scaleEdit.maximumSize=new Dimension(70,30)
	scaleEdit.preferredSize=scaleEdit.maximumSize
	scaleEdit.font=smallFont
	relativeEdit.font=smallFont
	
	listenTo(zoomAllBut,zoomInBut,zoomOutBut,relativeEdit)
	
	reactions += {
		case ButtonClicked(`zoomAllBut`)=> controller.zoomAll
		case ButtonClicked(`zoomInBut`)=> controller.zoomInClicked
		case ButtonClicked(`zoomOutBut`)=> model.zoomOut
		
		case EditDone(`relativeEdit`)=> {
			val scaleValue=splitScaleText(relativeEdit.text)
			if(scaleValue!=null){
				model.relativeScale=scaleValue
			}
		}
	}
	
	model.registerScaleListener(()=>{
		val sc=model.getScaleRatio
		scaleEdit.text=scaleToText(sc._1)+" : "+scaleToText(sc._2)
	})
	background=Color.gray
	
	contents+=zoomAllBut+=zoomInBut+=zoomOutBut +=Swing.HGlue+= new Label("Anzeige-Masstab: ")+=
		scaleEdit+=Swing.HStrut(20)+=new Label("Bezugs-Masstab: ")+=relativeEdit
		
	def scaleToText(sc:Number):String= sc match {
		case a:Integer => a.toString
		case b:java.lang.Double => "%4.2f".format(b)
		case c=> "u "+c
	}
	
	def splitScaleText(text:String):(Double,Double) = {
		val nseq=text.split(":")
		if(nseq.size!=2) null
		else {
			(nseq(0).trim.toDouble,nseq(1).trim.toDouble)
		}
	}
}