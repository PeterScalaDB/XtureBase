/**
 * Author: Peter Started:07.11.2010
 */
package client.layout

import scala.swing._
import java.awt.Color

/**
 * 
 */
class MainBox extends Panel with ViewboxHolder {
	
	def layoutManager = peer.getLayout.asInstanceOf[ViewboxLayout]
  
	var _centerBox:Viewbox=null
	
	def centerBox:Viewbox=_centerBox
	
	def centerBox_=(newB:Viewbox)={
		//if(_centerBox!=null) remove(_centerBox)		
		_centerBox=newB
		//add(_centerBox)
	}
	
	def add(comp:Component)= peer.add(comp.peer)
	def remove(comp:Component)= peer.remove(comp.peer)
	
	opaque=true
	background=Color.lightGray
	
	
	override lazy val peer = new MainboxPeer 
		
	
  def replaceBox(oldBox:Viewbox,newBox:Viewbox) = {
	  println("replace "+newBox)
	  centerBox=newBox
	  revalidate
	  repaint
	}
	
	def deleteMe(oldBox:Viewbox):Boolean = false
	
	class MainboxPeer extends javax.swing.JPanel(new ViewboxLayout) with SuperMixin {
		def mainBox= MainBox.this
	}
}