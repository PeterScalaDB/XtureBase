/**
 * Author: Peter Started:28.10.2010
 */
package client.graphicsView

import client.dialog.AnswerPanel
import scala.swing._
import definition.typ.ParamAnswerDefinition
import definition.expression.VectorConstant
import scala.swing.event._
import java.awt.Dimension

/** answer panel to give answers to point questions 
 * 
 */
class PointAnswerPanel extends AnswerPanel with PointClickListener {
  val bracketBut=new ToggleButton(" ( ")
	val globalBut = new Button("Gl")
  val dxBut = new Button("Dx")
  val dyBut = new Button("Dy")
  val midBut = new Button("Mi")
  val divBut = new Button("Te")
  val interBut = new Button("Sp")
  val textLabel=new Label()
  val textEdit=new TextField()
  var active=false
  
  textEdit.maximumSize=new Dimension(100,25)
  textEdit.preferredSize=new Dimension(100,25)
  
  dxBut.focusable=false
  dyBut.focusable=false
  midBut.focusable=false
  divBut.focusable=false
  bracketBut.focusable=false
  globalBut.focusable=false
  interBut.focusable=false
  
  
  var editingBut:Button=null
  
  
  textEdit.visible=false
  contents+=infoLabel += Swing.RigidBox(new Dimension(10,0))+= bracketBut +=globalBut +=
  dxBut += dyBut += midBut +=divBut+=interBut+=textLabel+=Swing.RigidBox(new Dimension(10,0))+=textEdit+=Swing.HGlue
  listenTo(bracketBut,globalBut,dxBut,dyBut,midBut,divBut,interBut,textEdit)
  
  reactions += {
  	case ButtonClicked(`bracketBut`) => {
  		if(bracketBut.selected) { // start
  			PointAnswerPanel.currentViewController.startBracketMode
  		}
  		else { // stop
  		  PointAnswerPanel.currentViewController.stopBracketMode	
  		}  		
  	}
  	case ButtonClicked(`dxBut`)=> {
  		textLabel.text="dx-Wert:"
  		textLabel.visible=true
  		textEdit.visible=true 
  		textEdit.selectAll
  		textEdit.requestFocusInWindow
  		editingBut=dxBut
  	}
  	case ButtonClicked(`dyBut`)=> {
  		textLabel.text="dy-Wert:"
  		textLabel.visible=true
  		textEdit.visible=true  
  		textEdit.selectAll
  		textEdit.requestFocusInWindow
  		editingBut=dyBut
  	}
  	case ButtonClicked(`globalBut`)=> {
  		textLabel.text="Koordinate: x ; y "
  		textLabel.visible=true
  		textEdit.visible=true  
  		textEdit.selectAll
  		textEdit.requestFocusInWindow
  		editingBut=globalBut
  	}
  	
  	case EditDone(`textEdit`) => {
  		println("edit done "+editingBut)
  		if(textEdit.visible) {
  			textLabel.visible=false
  			textEdit.visible=false
  			PointAnswerPanel.currentViewController.requestFocus
  			editingBut match {
  				case `dxBut` => PointAnswerPanel.currentViewController.addDelta(getTextEditDouble,0)  			
  				case `dyBut` => PointAnswerPanel.currentViewController.addDelta(0,getTextEditDouble) 
  				case `globalBut` => {
  					val z=textEdit.text.trim.split(";")
  					if(z.length==2){
  						val xv=z(0).toDouble
  						val yv=z(1).toDouble
  						PointAnswerPanel.currentViewController.setCoordinate(xv,yv)
  					}
  					
  				}
  			}
  		}  		
  	}
  	
  }
  
  def getTextEditDouble = textEdit.text.trim.replace(',','.').toDouble
  
  override def loadParamAnswer(answerDesc:ParamAnswerDefinition) = {
  	super.loadParamAnswer(answerDesc)
  	active=true
  	println("set Active "+answerDesc.name)
  	if(PointAnswerPanel.currentViewController!=null) {
  		if(answerDesc.constraint =="LineTo")
  			PointAnswerPanel.currentViewController.askForLineTo(this)
  		else {
  			if(answerDesc.constraint =="Create")
  				PointAnswerPanel.currentViewController.selectModel.deselect(false)
  			PointAnswerPanel.currentViewController.askForPointClick(this)
  		}
  		
  	}
  		
  }
  
  override def reset()= {
  	println("pointpanel reset "+active)
  	super.reset
  	bracketBut.selected=false
  	if(active) {
  		
  		active=false
  		if(PointAnswerPanel.currentViewController!=null) 
  		PointAnswerPanel.currentViewController.cancelModus
  	}
  }
  
  def pointClicked(point:VectorConstant) = {
  	println("point answer "+point)
  	func(ansParm,point)
  }
}

object PointAnswerPanel {
	var currentViewController:GraphViewController=_
	
	
}