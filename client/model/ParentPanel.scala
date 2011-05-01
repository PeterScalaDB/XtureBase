/**
 * Author: Peter Started:22.04.2011
 */
package client.model

import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.BoxPanel
import scala.swing.Orientation
import java.awt.Dimension
import scala.swing.Swing
import scala.swing.event.ButtonClicked
import definition.data.Reference
import definition.typ.form.FormBox
import javax.swing.BorderFactory
import scala.swing.SplitPane
import java.awt.Color
import client.comm.ClientQueryManager
//import server.test.SimpleProfiler

/**
 * 
 */
class ParentPanel() extends BorderPanel {	  
	  var splitter:SplitPane=_
    val pathLineRenderer=new PathLineRenderer
    pathLineRenderer.background=new Color(210,210,215)
	  
    val subBorder=BorderFactory.createMatteBorder(4,2,4,2,Color.lightGray)
    pathLineRenderer.border=subBorder
		//add(new Label("Test"),BorderPanel.Position.Center)
		var parentRef:Reference=_
		val closeBut=new Button("^")
		val leftBut=new Button("<")
		val rightBut=new Button(">")
		leftBut.peer.putClientProperty("JComponent.sizeVariant", "mini");
		leftBut.peer.updateUI();
		rightBut.peer.putClientProperty("JComponent.sizeVariant", "mini");
		rightBut.peer.updateUI();
		leftBut.focusable=false
		rightBut.focusable=false
		closeBut.focusable=false
		closeBut.peer.putClientProperty("JComponent.sizeVariant", "small");
		closeBut.peer.updateUI();
		
		val leftPanel=new BoxPanel(Orientation.Vertical){
			contents +=leftBut+=rightBut+=Swing.VGlue+=closeBut
			preferredSize=new Dimension(32,20)
		}
		
		add(leftPanel,BorderPanel.Position.West)
		listenTo(closeBut)
		
		reactions += {
			case ButtonClicked(`closeBut`)=>if (splitter!=null)splitter.dividerLocation=0
		}
		
		var forms:Option[FormBox]=None		
		
		def setForms(nf:Option[FormBox],indent:Int,nparentRef:Reference):Unit = {
			forms=nf
			parentRef=nparentRef
			
			nf match {
				case Some(fb )=> {
					//visible=true
					val fheight=fb.preferredSize.height
					leftBut.visible=fheight>90
					rightBut.visible=fheight>60
					closeBut.visible=false//fheight>40					
					fb.border=subBorder					
					add(fb,BorderPanel.Position.Center)					
					repaint
				}
				case None => {
					leftBut.visible=false
					rightBut.visible=false
					closeBut.visible=false
					add(pathLineRenderer,BorderPanel.Position.Center )
					val instList=ClientQueryManager.queryInstance(parentRef,-1)
					if(instList.isEmpty) println("ParentPanel  parentRef does not exist:"+parentRef)
					else pathLineRenderer.config(true, true, instList.first, indent)
					//visible=false
				}
			}
		}
		
		def setParentHeight(nh:Int)= {
			pathLineRenderer.preferredSize=new Dimension(10,nh-2)
		}

}