/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import scala.swing._
import collection.mutable.ArrayBuffer
import definition.typ._
import definition.expression.Constant

/**
 * 
 */
class AnswerArea extends BoxPanel(scala.swing.Orientation.Vertical ) {
  val boolPool=new PanelPool[BoolAnswerPanel]
  val intPool=new PanelPool[IntAnswerPanel]
  val stringPool=new PanelPool[StringAnswerPanel]
  val poolArray=Array(boolPool,intPool,stringPool)  
  var func: (ParamAnswerDefinition,Constant)=>Unit = _
  
  
  def reset()= {
  	poolArray foreach( _.reset)
  	contents.clear
  }
  
  def loadAnswerDefinitions(question:ParamQuestion) = {
  	reset()
  	for(ans <-question.possibleAnswers ) {
  		val panel= ans dataType match {
  			case DataType.BoolTyp =>  boolPool.getPanel()
  			case DataType.IntTyp =>  intPool.getPanel()
  			case DataType.LongTyp => intPool.getPanel()
  			case DataType.StringTyp => stringPool.getPanel()
  			case a => throw new IllegalArgumentException ("Answertype "+a+" not supported ")
  		}
  		panel.loadParamAnswer(ans)
  		contents+=panel
  	}
  	revalidate	
  }
  
  def registerAnswerCallBack(nfunc: (ParamAnswerDefinition,Constant)=>Unit) = 
  {
  	func=nfunc
  }
  
  class PanelPool [T <: AnswerPanel](implicit m: scala.reflect.Manifest[T]) {
	val pool=ArrayBuffer[T]()
	var usedPanels=0
	def getPanel() = {
		usedPanels+= 1
		if(pool.size>=usedPanels) pool(usedPanels-1)
		else {
			val newPan = m.erasure.newInstance.asInstanceOf[T]
			newPan.registerAnswerCallBack(func)
			pool append newPan
			newPan
		}
	}
	def reset = usedPanels=0	
}
}

