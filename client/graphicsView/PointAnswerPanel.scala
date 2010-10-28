/**
 * Author: Peter Started:28.10.2010
 */
package client.graphicsView

import client.dialog.AnswerPanel
import scala.swing._
import definition.typ.ParamAnswerDefinition
import definition.expression.VectorConstant

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
  
  
  textEdit.visible=false
  contents+=infoLabel += Swing.RigidBox(new Dimension(10,0))+= bracketBut +=globalBut +=
  dxBut += midBut +=divBut+=interBut+=textLabel+=textEdit+=Swing.HGlue
  listenTo(bracketBut,globalBut,dxBut,midBut,divBut,interBut,textEdit)
  
  override def loadParamAnswer(answerDesc:ParamAnswerDefinition) = {
  	super.loadParamAnswer(answerDesc)
  	active=true
  	println("set Active "+answerDesc.name)
  	if(PointAnswerPanel.currentViewController!=null) 
  		PointAnswerPanel.currentViewController.askForPointClick(this)
  }
  
  override def reset()= {
  	println("pointpanel reset")
  	super.reset  	
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