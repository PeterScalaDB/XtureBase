/**
 * Author: Peter Started:06.11.2010
 */
package client.layout


import scala.swing._
import scala.swing.event._
import java.awt.{Font,Color}
/**
 * 
 */
trait Stripe {	
  def baseBox:Viewbox
  def isHorizontal:Boolean
}

object Stripe {
	type StripeType=Panel with Stripe
	val butFont=new Font("Arial",0,10)
}

class ExpandStripe(val isHorizontal:Boolean,var baseBox:Viewbox) extends 
	BoxPanel(if(isHorizontal)Orientation.Horizontal else Orientation.Vertical) with Stripe{
	
	val butList=ViewboxContentTypeList.list.map(  new ExpandButton( _ ))
	//println("Types:"+ViewboxContentTypeList.list.mkString)
	contents+=(if(isHorizontal)Swing.HStrut(10) else Swing.VStrut(10))
	contents ++=butList
	contents+=(if (isHorizontal)Swing.HGlue else Swing.VGlue)
	listenTo(butList:_*)
	reactions += {
		case ButtonClicked(e:ExpandButton)  => {
			val newContent=e.typeInfo.factory()
			if(isHorizontal) {
			  baseBox.bottomEdge.connectTo(newContent)	
			}else {
				baseBox.rightEdge.connectTo(newContent)
			}			
		}
	}
	
}




class ConnectorStripe(val isHorizontal:Boolean,val baseBox:Viewbox,var connectedBox:Viewbox) extends 
  BoxPanel(if(isHorizontal)Orientation.Horizontal else Orientation.Vertical) with Stripe {
	
	val syncRightBut=new Button(">")
	val syncLeftBut=new Button("<")
	var scaleValue:Double= -1
	//val scaleEdit=new TextField(scaleValue.toString)	
	val dragArea=new DragArea(isHorizontal,this)
	var dragScale:Double= -1
	var defaultScale:Double= -1
	
	private var baseSize:Int= -1
	private var maxSize:Double= 0
	
	opaque=true
	background=Color.lightGray
	syncRightBut.margin=new Insets(0,0,0,0)	
	syncLeftBut.margin=new Insets(0,0,0,0)
	syncRightBut.font=Stripe.butFont
	syncLeftBut.font=Stripe.butFont
	syncRightBut.focusable=false
	syncLeftBut.focusable=false
	/*scaleEdit.maximumSize=new Dimension(40,30)
	//scaleEdit.preferredSize=maximumSize
	scaleEdit.preferredSize=new Dimension(30,30)
	scaleEdit.font=new Font("Arial",0,8)*/
	contents+=(if(isHorizontal)Swing.HStrut(10) else Swing.VStrut(10))+=syncRightBut+=syncLeftBut+=dragArea	
		//(if (isHorizontal)Swing.HGlue else Swing.VGlue)
	
	/*listenTo(scaleEdit)
	reactions+= {
		case e:EditDone => {
			scaleValue=e.source.text.trim.replace(',','.').toDouble
			baseBox.revalidate
		}
	}	*/
		
	def connectWith(newConnectedBox:Viewbox) = {
		connectedBox=newConnectedBox
	}
	
	
	def dragStopped = {
		//println("drag stopped dragScale:"+dragScale)
		if(dragScale!= -1) scaleValue=dragScale
		dragScale= -1
		revalidate
	}
	
	def dragTo(delta:Int) = {
		if(dragScale == -1 ) { // start Drag
		  baseSize= if(isHorizontal) baseBox.bounds.height
		  else baseBox.bounds.width
		  val scale=if(scaleValue == -1 ) defaultScale else scaleValue
		  maxSize=baseSize.toDouble/scale
		  //println("Start: scale:"+scale+" baseSize:"+baseSize+" maxSize:"+maxSize)
		}		
		
		
		val newSize=baseSize+delta
		
		if(newSize>30 && newSize<maxSize-30) {
			dragScale=newSize/maxSize
			//println(" delta:"+delta+" newSize:"+newSize+" dragScale:"+dragScale)
			revalidate
		}		
	}
	
}



class ExpandButton(val typeInfo:ViewboxContentType) extends Button(typeInfo.buttonText) {
	margin=new Insets(0,0,0,0)
	focusable=false
}
