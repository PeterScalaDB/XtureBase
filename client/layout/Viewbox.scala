/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import scala.swing._
import scala.swing.event._
import java.awt.{Color}
import javax.swing.border._
import javax.swing.{BorderFactory}

/**
 * 
 */

trait ViewboxHolder {
	def replaceBox(oldBox:Viewbox,newBox:Viewbox) 
	def deleteMe(oldBox:Viewbox):Boolean
}

class Viewbox(val mainbox:MainBox,val showCloseButton:Boolean,var holder:ViewboxHolder) extends BorderPanel{
	
	val rightEdge =new BoxEdge(this,new ExpandStripe(false,this))
	val bottomEdge = new BoxEdge(this,new ExpandStripe(true,this))
	//var leftEdge:Option[BoxEdge]=None
	//var topEdge:Option[BoxEdge]=None
	var content: ViewboxContent#CompType =null	
  val header = new ViewboxHeader(this)
	val widthSet= -1
	val heightSet= -1
	
	var minimizeHeaderCallBack:()=>Unit =null
	
	
	
	var rightFirstExpanded=false
  border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);

	add(header,BorderPanel.Position.North)
	mainbox.add(this)
	
	
	override def add(comp:Component,con:Constraints)= super.add(comp,con)
	
	def addContent(newContent:ViewboxContent#CompType)= {
		if(content!=null) content.close()
		add(newContent,BorderPanel.Position.Center)		
		content=newContent		
		content.setViewbox(this)
		content.open()
		mainbox.revalidate
		repaint
	}
	
	def foreach(func:(Viewbox)=>Unit):Unit = {
		func(this)
		if(rightEdge.isExpanded) rightEdge.getConnectedBox.foreach(func)
		if(bottomEdge.isExpanded) bottomEdge.getConnectedBox.foreach(func)
	}
	
	
	override def preferredSize = {
		val superSize=super.preferredSize		
		new Dimension(if(widthSet>0)widthSet else superSize.width,
			if(heightSet>0)heightSet else superSize.height)
	}
	
	def getFullPrefSize:Dimension = {
		val prefS=preferredSize
		var w=prefS.width
		var h=prefS.height
		var rightSize:Dimension=null
		var botSize:Dimension=null
		//System.out.println ("getFullPrefSize "+header.label.text+" rightFirst:"+rightFirstExpanded+" w:"+w+" h:"+h)
		if(rightEdge.isExpanded){
			rightSize=rightEdge.connectorStripe.get.connectedBox.getFullPrefSize
			if(!rightFirstExpanded && rightSize.height>h) h=rightSize.height			
		}
		if(bottomEdge.isExpanded) {
			botSize=bottomEdge.connectorStripe.get.connectedBox.getFullPrefSize
			if(rightFirstExpanded && botSize.width>w) w=botSize.width
			h+= botSize.height
		}
		if(rightEdge.isExpanded) w+= rightSize.width
		if(!rightFirstExpanded && botSize!=null && botSize.width>w) w=botSize.width
		if(rightFirstExpanded && rightSize!=null && rightSize.height>h)h=rightSize.height
		
		//System.out.println("result "+header.label.text+" w:"+w+" h:"+h)
		return new Dimension(w,h)
	}
	
	def close:Unit = {
		var doubleConnected=false
		val replaceBox= if(rightEdge.isExpanded) {
			if(bottomEdge.isExpanded){
				doubleConnected=true
				if(rightFirstExpanded) bottomEdge.getConnectedBox
				else rightEdge.getConnectedBox
			} 
			else rightEdge.getConnectedBox
		} else if(bottomEdge.isExpanded) bottomEdge.getConnectedBox
		else null
		if(replaceBox!=null) {
			if(doubleConnected) {
				if(rightEdge.getConnectedBox.isDoubleConnected) {
					if(bottomEdge.getConnectedBox.isDoubleConnected) return
					else { // bottom box has free edge, replace this by bottom box						
						// find free edge in bottom box
						val bottomBox=bottomEdge.getConnectedBox						
						val freeEdge =  if(bottomBox.rightEdge .isExpanded) bottomBox.bottomEdge
							else bottomBox.rightEdge
						holder.replaceBox(this,bottomBox)
						bottomBox.holder=holder
						freeEdge.connectBox(rightEdge.getConnectedBox)
						rightEdge.getConnectedBox.holder=freeEdge
					}
				} else {
					// right box has free edge, replace this by right box
					val rightBox=rightEdge.getConnectedBox
					val freeEdge =  if(rightBox.rightEdge .isExpanded) rightBox.bottomEdge
							else rightBox.rightEdge
					holder.replaceBox(this,rightBox)
					rightBox.holder=holder
					freeEdge.connectBox(bottomEdge.getConnectedBox)
					bottomEdge.getConnectedBox.holder=freeEdge
				}
			}else {
			  holder.replaceBox(this,replaceBox)
			  replaceBox.holder=holder	
			}
		  
		} else if(!holder.deleteMe(this)) return
		content.storeSettings()
		content.close()		
		//add(null,BorderPanel.Position.Center)
		content=null
		mainbox.remove(this)
		mainbox.revalidate		
		mainbox.repaint
	}
	
	def storeSettings() = {
		content.storeSettings()
	}
	
	def isDoubleConnected=rightEdge.isExpanded&&bottomEdge.isExpanded
	
	def setTitle(title:String)= header.setTitle(title)
	
	def minimizeHeaderPanel(callBack:()=>Unit) = {
		minimizeHeaderCallBack=callBack
		header.openContentHeaderBut.visible=true
	}
	
}

class ViewboxHeader(viewbox:Viewbox) extends BoxPanel(Orientation.Horizontal) {
	opaque=true
	background=Color.gray
	preferredSize=new Dimension(100,30)
	val label=new Label
	label.foreground=Color.white
	val openContentHeaderBut=new Button("\u02c5")
	val closeBut=new Button("X")
	closeBut.margin=new Insets(0,0,0,0)
	openContentHeaderBut.margin=closeBut.margin
	openContentHeaderBut.visible=false
	openContentHeaderBut.focusable=false
	closeBut.focusable=false
	//if(viewbox.showCloseButton)
		contents +=openContentHeaderBut+=Swing.HStrut(10)+=label+=Swing.HGlue+=Swing.HStrut(10)+=closeBut
	//	else contents+=label+=Swing.HGlue
	listenTo(closeBut,openContentHeaderBut)
	reactions += {
			case ButtonClicked(`closeBut`) => {
				viewbox.close
			}
			case ButtonClicked(`openContentHeaderBut`) => {
				openContentHeaderBut.visible=false
				viewbox.minimizeHeaderCallBack()
			}
		}
	
	def setTitle(newTitle:String)= label.text=newTitle
	
}



class BoxEdge(val sourceBox:Viewbox,val expandStripe:ExpandStripe) extends ViewboxHolder   {
	var connectorStripe:Option[ConnectorStripe]=None
	private var expanded:Boolean=false
	//var currentStripe:Stripe.StripeType=expandStripe
	sourceBox.add(expandStripe,getEdgePos)
	
	def isExpanded=expanded
	
	def connectTo(content:ViewboxContent#CompType) = {
		
		//sourceBox.mainbox.remove(currentStripe)
		val newBox=new Viewbox(sourceBox.mainbox,true,this)
		newBox.addContent(content)
		connectBox(newBox)
	}
	
	def connectBox(newBox:Viewbox)= {
		val conn=new ConnectorStripe(isHorizontal,sourceBox,newBox)		
		if(!expanded && !conn.isHorizontal && !sourceBox.bottomEdge .expanded) sourceBox.rightFirstExpanded=true
		connectorStripe=Some(conn)
		sourceBox.add(conn,getEdgePos)
		//currentStripe=conn
		expanded=true
		sourceBox.mainbox.revalidate
	}
	
	def disconnect() = if(expanded) {
		//sourceBox.mainbox.remove(currentStripe)
		if(!expandStripe.isHorizontal) sourceBox.rightFirstExpanded=false
		//currentStripe=expandStripe
		
		connectorStripe=None
	  sourceBox.add(expandStripe,getEdgePos)	
		expanded=false
	}
	
	def isHorizontal=expandStripe.isHorizontal
	def getEdgePos=if(isHorizontal)
		BorderPanel.Position.South else BorderPanel.Position.East
	
	def getConnectedBox= connectorStripe.get.connectedBox 	
		
	def replaceBox(oldBox:Viewbox,newBox:Viewbox) = {
			if(isExpanded){
				connectorStripe.get.connectedBox=newBox
			}
		}
	
	def deleteMe(oldBox:Viewbox) = {
	    if(isExpanded){
	    	disconnect()
	    	true
			}	else false
	}
}

object Viewbox extends SimpleSwingApplication  {
	var counter:Int=0
	def getCounter= {counter +=1;counter }
	
	class ContBox extends BorderPanel with ViewboxContent {			
			opaque=true
			background=new Color((Math.random*(255*255*255).toDouble).toInt)
			preferredSize=new Dimension(100,100)
			def open() = {
				System.out.println("open ")
			}
			def close() = {
				System.out.println("close ")
			}
			def storeSettings() = {
				System.out.println("store settings")
			}
			def getType=contBoxType			
			def setViewbox(box:Viewbox) = {box.setTitle("Test-Box "+getCounter)}
			def typeID:String = "Test"
		}
	
	val contBoxType=new ViewboxContentType(1,"testType","T",()=>{
		new ContBox
	})
	ViewboxContentTypeList.addType(contBoxType)
	
	val contBox=new ContBox
	val mainbox=new MainBox
	val myViewbox=new Viewbox(mainbox,false,mainbox)
	
	
	
	val top = new MainFrame ()
	{		
		title="ViewboxTest"			  
		myViewbox.addContent(contBox)
		mainbox.centerBox=myViewbox
		contents = mainbox
		bounds=new Rectangle(20,20,1000,700)
	}	
	
	
}