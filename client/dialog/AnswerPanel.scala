/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import definition.typ._
import definition.expression._
import scala.swing._
import scala.swing.event._
import java.awt.Font
import javax.swing.BorderFactory


/** abstract superclass for all answer panels
 * 
 */
abstract class AnswerPanel extends BoxPanel (Orientation.Horizontal) {
	val labelFont=new Font("Arial",0,14)
	val infoLabel=new Label()
	infoLabel.font=labelFont
	infoLabel.border=BorderFactory.createEmptyBorder(2,3,2,3)
	var func: (ParamAnswerDefinition,Constant)=>Unit = _
	var ansParm:ParamAnswerDefinition = _
	
	def loadParamAnswer(answerDesc:ParamAnswerDefinition) = {
		ansParm=answerDesc
		//reset()
		infoLabel.text=answerDesc.name
	}
	def registerAnswerCallBack(nfunc: (ParamAnswerDefinition,Constant)=>Unit) = func=nfunc
	def reset()={}
	def setFocus()
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
	def setFocus={}
}

class StringAnswerPanel extends  AnswerPanel {	
	val textField=new TextField("")
	//val okBut = new Button("ok")	
	override def loadParamAnswer(answerDesc:ParamAnswerDefinition)= {
		super.loadParamAnswer(answerDesc)
		textField.requestFocusInWindow
	}
	contents +=infoLabel+= Swing.RigidBox(new Dimension(10,0))+=textField+=Swing.HGlue
	maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
	listenTo(textField.keys)	
	reactions+= {
		case KeyPressed(_, Key.Enter, _, _) => {				
			editDone				
		}
	}	
	def editDone = {
		System.out.println("Edit done "+this.getClass)
		func(ansParm,new StringConstant(textField.text))
	}
	override def reset()= textField.text=""
	def setFocus()= textField.requestFocus
}



class IntAnswerPanel extends  StringAnswerPanel {		
	
		override def editDone ={
			try {					
				System.out.println("Edit done Int :"+this.getClass)
				val tx=textField.text.trim
				if(tx.size>0)
					func(ansParm,new IntConstant(tx.toInt))	
			}
			catch {
				case ex:Exception => System.err.println (ex)
			}																		
		}		
}


class DoubleAnswerPanel extends  StringAnswerPanel {		
	override def editDone= {
		try {
			val tx=textField.text.trim.replace(',','.')
			if(tx.size>0)
				func(ansParm,new DoubleConstant(tx.toDouble))	
		}
		catch {
			case ex:Exception => System.err.println(ex)
		}	
	}	
}	
