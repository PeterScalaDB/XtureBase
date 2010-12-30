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
  val doublePool=new PanelPool[DoubleAnswerPanel]
  val stringPool=new PanelPool[StringAnswerPanel]
  val poolArray=Array(boolPool,intPool,stringPool,doublePool) 
  
  val customPools=new collection.mutable.HashMap[DataType.Value,PanelPool[_ <: AnswerPanel]]
  
  var func: (ParamAnswerDefinition,Constant)=>Unit = _
  
  
  
  def reset()= {
  	//System.out.println("answerArea reset")
  	poolArray foreach( _.reset)
  	customPools.values.foreach( _.reset)
  	contents.clear
  	revalidate
  	repaint
  }
  
  def loadAnswerDefinitions(question:DialogQuestion) = {
  	//reset()
  	contents.clear
  	var firstPanel=true
  	for(ans <-question.possibleAnswers ) {
  		val panel= ans dataType match {
  			case DataType.BoolTyp =>  boolPool.getPanel()
  			case DataType.IntTyp =>  intPool.getPanel()
  			case DataType.LongTyp => intPool.getPanel()
  			case DataType.DoubleTyp => doublePool.getPanel()
  			case DataType.StringTyp => stringPool.getPanel()
  			case a =>  			
  				if(customPools.contains(a)) {
  					val custPool=customPools(a)
  					custPool.getPanel()
  				}
  				else throw new IllegalArgumentException ("Answertype "+a+" not supported ")
  		}
  		panel.loadParamAnswer(ans)  		
  		contents+=panel
  		if(firstPanel){
  			firstPanel=false
  			panel.setFocus()
  		}
  	}
  	revalidate	
  	repaint
  }
  
  def registerAnswerCallBack(nfunc: (ParamAnswerDefinition,Constant)=>Unit) = 
  {
  	func=nfunc
  }
  
  def registerCustomPanel[T <: AnswerPanel](typ:DataType.Value)(implicit m: scala.reflect.Manifest[T]) = {
  	customPools(typ)=new PanelPool[T]
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
  	def reset = {
  		for(i <- 0 until usedPanels) pool(i).reset
  		usedPanels=0  		
  	}
  }
}

