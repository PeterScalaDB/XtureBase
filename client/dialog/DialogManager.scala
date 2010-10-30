/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import scala.swing._
import definition.typ._
import definition.data._
import client.dataviewer._
import definition.expression.Constant
import client.comm.ClientQueryManager
import javax.swing.{BorderFactory}
import javax.swing.border._ 
import scala.swing.event._

/** manages the user dialog and provides the DialogArea panel
 * 
 */

case class ResultElement(val question:ParamQuestion,val answer:ParamAnswerDefinition,val result:Constant)

object DialogManager extends SelectListener with ActionPanListener{
	
	val questionField=new Label()	
	val errorField=new Label()	
	val answerArea=new AnswerArea ()
	val cancelBut=new Button("Abbrechen")
	cancelBut.visible=false
	answerArea.registerAnswerCallBack(answerGiven)
	
	var selectedInstances:Seq[Referencable] = _
	var actionInstances:Seq[Referencable] = _
	
	var currentQuestion:ParamQuestion= _
	var repeatQuestion:ParamQuestion= _
	var isRepeating=false
	var currentAction:String= _
	val answerList=new scala.collection.mutable.ArrayBuffer[ResultElement]()
	var createType:Int=0
	//var newFunc:(Seq[(String,Constant)])=>Unit = null
	
  val dialogPanel = new BoxPanel(Orientation.Horizontal ) {
		border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
     contents+= new GridPanel(2,1) {
    	 contents+= questionField+=new BoxPanel(Orientation.Horizontal) {
    		contents+=errorField+=cancelBut 
    	 }
     }
     contents+=new ScrollPane() {
    	 viewportView=answerArea
     }
     listenTo(cancelBut)
     reactions+= {
    	 case ButtonClicked(`cancelBut`) => reset
     }
     preferredSize=new Dimension(0,70)
  }
	
	def reset()= {
		//println("dialogmanager reset "+isRepeating)
		if(isRepeating) { // when stopped during repeating, keep the results that are already there
			isRepeating=false
			processResults
		}
		answerArea.reset()
		questionField.text=""
		errorField.text=""	
		cancelBut.visible=false
		
	}
	// from DataViewController selection listener 
	def selectionChanged(sender:SelectSender,instList:Seq[Referencable]) = {
		selectedInstances=instList
		//println("diaman sel changed")
		reset()
		if(selectedInstances==null || selectedInstances.isEmpty) questionField.text="Keine Objekte ausgewählt"
		else {
			val typ=AllClasses.get.getCommonClass(selectedInstances)
			questionField.text=(if(selectedInstances.size==1)"1 " else (selectedInstances.size.toString+" "))+
			  (if(typ== -1) "" else AllClasses.get.getClassByID(typ).name+" - ")+"Objekt"+(if(selectedInstances.size>1)"e " else " ")+
			  "ausgewählt"
		}
	}
	
	
	// from ActionPanel listener	
	def startActionDialog(actionName:String,question:ParamQuestion,actionInst:Seq[Referencable],ncreateType:Int) = {
		actionInstances=actionInst
		createType=ncreateType
		currentAction=actionName
		repeatQuestion=null
		isRepeating=false
		answerList.clear
		loadQuestion(question)
		cancelBut.visible=true
		//newFunc=null
	}	
	
		
	/*def startNewDialog(actionName:String,question:ParamQuestion,func:(Seq[(String,Constant)])=>Unit) = {
		currentAction=actionName
		repeatQuestion=null
		isRepeating=false
		answerList.clear
		loadQuestion(question)
		cancelBut.visible=true
		newFunc=func
	}*/
	
	private def loadQuestion(question:ParamQuestion) = {
		currentQuestion=question
		if(question.repeat) repeatQuestion=question
		questionField.text=question.name
		errorField.text=""
		answerArea.loadAnswerDefinitions(question)
	}
	
	def answerGiven(parm:ParamAnswerDefinition,result:Constant):Unit = {
		answerList += ResultElement(currentQuestion,parm,result)		
		if(parm.followQuestion ==None) 
			if(repeatQuestion!=null){
				isRepeating=true
				loadQuestion(repeatQuestion)
			}
			else { // action is ready				
				processResults
				reset()
			}		
		else {			
			loadQuestion(parm.followQuestion.get)
		}
	}
	
	def processResults = {
		val resultList=answerList.map(x =>(x.answer.name,x.result))			
		/*if(newFunc!=null)  // create new element
			newFunc(resultList)
		else*/ if(actionInstances!=null) {
			if(createType==0) // modification action
		  ClientQueryManager.executeAction(actionInstances,currentAction,resultList)
		  // propertyField set to 0 because simpleCreateActions are handled in ActionPanel
		  else ClientQueryManager.executeCreateAction(actionInstances,createType,0,currentAction,resultList)
		}
					
	}
	
	
}