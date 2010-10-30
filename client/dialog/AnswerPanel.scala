/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import definition.typ._
import definition.expression._
import scala.swing._
import scala.swing.event._


/** abstract superclass for all answer panels
 * 
 */
abstract class AnswerPanel extends BoxPanel (Orientation.Horizontal) {	
	val infoLabel=new Label()
	var func: (ParamAnswerDefinition,Constant)=>Unit = _
	var ansParm:ParamAnswerDefinition = _
	
	def loadParamAnswer(answerDesc:ParamAnswerDefinition) = {
		ansParm=answerDesc
		//reset()
		infoLabel.text=answerDesc.name
	}
	def registerAnswerCallBack(nfunc: (ParamAnswerDefinition,Constant)=>Unit) = func=nfunc
	def reset()= {}
}

/** Answerpanel for Boolean result values
 * 
 */
class BoolAnswerPanel  extends AnswerPanel {	
	val yesBut=new Button("Ja")
	val noBut = new Button("Nein")	
	contents +=infoLabel+= Swing.RigidBox(new Dimension(10,0))+=yesBut+=Swing.RigidBox(new Dimension(10,0))+=noBut
	listenTo(yesBut,noBut)	
	reactions+= {
		case ButtonClicked(e) => func(ansParm,new BoolConstant(e.text=="Ja"))															
	}
}

class StringAnswerPanel extends  AnswerPanel {	
	val textField=new TextField("")
	//val okBut = new Button("ok")	
	override def loadParamAnswer(answerDesc:ParamAnswerDefinition)= {
		super.loadParamAnswer(answerDesc)
		textField.requestFocusInWindow
	}
	contents +=infoLabel+= Swing.RigidBox(new Dimension(10,0))+=textField+=Swing.HGlue
	listenTo(textField)
	initReactions()
	
	def initReactions()= {
		reactions+= {
			case EditDone(e) =>  func(ansParm,new StringConstant(textField.text))				
		}	
	}
	override def reset()= textField.text=""
}



class IntAnswerPanel extends  StringAnswerPanel {		
	override def initReactions() = {
		reactions+= {
			case EditDone(e) => {
				try {
					val tx=textField.text.trim
					if(tx.size>0)
					func(ansParm,new IntConstant(tx.toInt))	
				}
				catch {
					case ex:Exception => println(ex)
				}																		
			}
		}	
	}
}


class DoubleAnswerPanel extends  StringAnswerPanel {		
	override def initReactions() = {
		reactions+= {
			case EditDone(e) => {
				try {
					val tx=textField.text.trim.replace(',','.')
					if(tx.size>0)
					func(ansParm,new DoubleConstant(tx.toDouble))	
				}
				catch {
					case ex:Exception => println(ex)
				}																		
			}
		}	
	}	
}