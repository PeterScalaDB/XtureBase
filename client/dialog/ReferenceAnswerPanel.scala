/**
 * Author: Peter Started:28.05.2011
 */
package client.dialog
import definition.data.Reference
import scala.swing._
import definition.typ.ParamAnswerDefinition
import definition.expression.ObjectReference
/**
 * 
 */

trait ObjectSelectListener{
	def objectsSelected(objs:ObjectReference)
}

class ReferenceAnswerPanel extends AnswerPanel with ObjectSelectListener {
  val filterBut=new Button("Filter")
  val cuttedBut=new ToggleButton("Geschn.")
  val onlyFullBut=new ToggleButton("Nur Voll.")
  //val textLabel=new Label()
  var active=false
  
  filterBut.focusable=false
  cuttedBut.focusable=false
  onlyFullBut.focusable=false
  
  contents+=infoLabel += Swing.RigidBox(new Dimension(10,0))+=filterBut+=cuttedBut+=onlyFullBut
  
  listenTo(filterBut,cuttedBut,onlyFullBut)
  
  override def loadParamAnswer(answerDesc:ParamAnswerDefinition) = {
  	super.loadParamAnswer(answerDesc)
  	active=true
  	System.out.println("set Active "+answerDesc.name)
  	if(ReferenceAnswerPanel.currentViewController!=null) {
  		System.out.println("AnswerPanel constraint:"+answerDesc.constraint)
  		ReferenceAnswerPanel.currentViewController.askForObjectSelection(this,answerDesc.constraint)  		
  	}  		
  }
  
 
  
  override def reset()= {
  	System.out.println("Refpanel reset "+active)
  	super.reset  	
  	if(active) {  		
  		active=false
  		if(ReferenceAnswerPanel.currentViewController!=null) 
  			ReferenceAnswerPanel.currentViewController.cancelModus
  	}
  }
  
  def objectsSelected(objs:ObjectReference) = {
  	func(ansParm,objs)
  }
  
  
  
  def setFocus()={}
  
  
  

}

object ReferenceAnswerPanel {
	var currentViewController:AbstractViewController=_	
	
}