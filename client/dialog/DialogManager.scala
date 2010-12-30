/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import scala.swing._
import definition.typ._
import definition.data._
import client.dataviewer._
import definition.expression.Constant
import client.comm.{ClientQueryManager,ErrorListener}
import javax.swing.{BorderFactory,JComponent,KeyStroke}
import java.awt.event.{KeyEvent}
import javax.swing.border._ 
import scala.swing.event._
import java.awt.Font
import client.print.PrintQuestionHandler

/** manages the user dialog and provides the DialogArea panel
 * 
 */

case class ResultElement(val question:ParamQuestion,val answer:ParamAnswerDefinition,val result:Constant)

object DialogManager extends SelectListener with ActionPanListener{
	
	val lock=new Object
	var dialogIsActive=false
	var isServerEnquiry=false
	val questionField=new Label()	
	val questionFont=new Font("Arial",0,14)
	questionField.font=questionFont
	questionField.border=BorderFactory.createEmptyBorder(0,3,3,0)
	val errorField=new TextArea()
	errorField.lineWrap=true
	errorField.wordWrap=true
	errorField.editable=false
	errorField.preferredSize=new Dimension(10,60)
	/*questionField.lineWrap=true
	questionField.wordWrap=true
	questionField.editable=false*/
	val errorListener=new ErrorListener () {
		def printError(errorText:String)= errorField.peer.append(errorText)
	}
	ClientQueryManager.registerErrorListener(errorListener)
	LogOutputStream.registerListener(errorListener)
	val answerArea=new AnswerArea ()
	val cancelBut=new Button("Abbrechen")
	cancelBut.visible=false
	answerArea.registerAnswerCallBack(answerGiven)
	
	var selectedInstances:Seq[SelectGroup[_<:Referencable]] = _
	var actionGroups:Seq[SelectGroup[_<:Referencable]] = _
	
	var currentQuestion:ParamQuestion= _
	var repeatQuestion:DialogQuestion= _
	var isRepeating=false
	var currentAction:String= _
	val answerList=new scala.collection.mutable.ArrayBuffer[ResultElement]()
	var createType:Int=0
	//var parent:OwnerReference= _
	//var newFunc:(Seq[(String,Constant)])=>Unit = null
	
	var customQuestionHandlerList=collection.mutable.HashMap[String,CustomQuestionHandler]()
	
  val dialogPanel = new BoxPanel(Orientation.Horizontal ) {
		errorField.background=background
		border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
		val errorScroller=new ScrollPane {
			viewportView=errorField
		}
    contents+= new BoxPanel(Orientation.Vertical) {
    	 contents+= questionField+=new BoxPanel(Orientation.Horizontal) {
    		contents+=errorScroller+=cancelBut 
    	 }
    	 preferredSize=new Dimension(340,80)
    	 maximumSize=preferredSize
     }
    contents+=new ScrollPane() {
    	 viewportView=answerArea
    }
    listenTo(cancelBut)
    reactions+= {
     case ButtonClicked(`cancelBut`) => reset
    }
    preferredSize=new Dimension(600,80)
    
    initKeyMappings
    
    def initKeyMappings:Unit = {
    	val iMap=answerArea.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
    	val cancelKey="Cancel"
    	iMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,0,false), cancelKey)
    	answerArea.peer.getActionMap.put(cancelKey,new javax.swing.AbstractAction() {
    		def actionPerformed(e:java.awt.event.ActionEvent)= {
    			//System.out.println("reset !!!")
    			reset
    		}
    	})
	}
  }
	ClientQueryManager.registerAfterListener(()=> {
	  initCustomQuestionHandlers	
	})
	
	
	
	
	def reset():Unit= lock.synchronized{
		//System.out.println("dialogmanager reset "+isRepeating)
		if(isRepeating) { // when stopped during repeating, keep the results that are already there
			isRepeating=false
			processResults
		}
		else if(isServerEnquiry){
			isServerEnquiry=false
			ClientQueryManager.answerEnquiry(Seq.empty)			
		}	
		answerArea.reset()
		questionField.text=""
		errorField.text=""	
		cancelBut.visible=false
		dialogPanel.revalidate
		dialogPanel.repaint
		dialogIsActive=false		
	}
	
	// from DataViewController selection listener 
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]]) = {
		selectedInstances=groups
		//System.out.println("diaman sel changed")
		reset()
		if(selectedInstances==null || selectedInstances.isEmpty) questionField.text="Keine Objekte ausgewählt"
		else {
			val typ=AllClasses.get.getCommonClassForGroups(selectedInstances)
			val numInstances=selectedInstances.foldLeft(0)(_+_.children.size)
			
			questionField.text=(if(numInstances==1)"1 " else (numInstances.toString+" "))+
			  (if(typ< 0) "" else AllClasses.get.getClassByID(typ).name+" - ")+"Objekt"+(if(numInstances>1)"e " else " ")+
			  "ausgewählt"
		}
	}
	
	
	// from ActionPanel listener	
	def startActionDialog(actionName:String,question:ParamQuestion,groupList:Seq[SelectGroup[_<:Referencable]],ncreateType:Int):Unit = lock.synchronized{
		if(dialogIsActive)reset
		isServerEnquiry=false
		if(question==null)return
		actionGroups=groupList
		createType=ncreateType
		//System.out.println("start Action Dialog createType:"+createType)
		currentAction=actionName
		repeatQuestion=null
		isRepeating=false
		answerList.clear		
		loadQuestion(question)
		cancelBut.visible=true
		dialogIsActive=true
		//newFunc=null
	}	
	
	def startEnquiryDialog(question:ParamQuestion):Unit = lock.synchronized{
		if(dialogIsActive)reset
		isServerEnquiry=true
		repeatQuestion=null
		isRepeating=false
		answerList.clear		
		loadQuestion(question)
		cancelBut.visible=true
		dialogIsActive=true
	}

	
	private def loadQuestion(question:ParamQuestion) = {
		currentQuestion=question
		question match {
			case q:DialogQuestion=> {
				if(q.repeat) repeatQuestion=q
				questionField.text=q.name
				errorField.text=""
					answerArea.loadAnswerDefinitions(q)
			}
			case c:CustomQuestion => {
				println("CustomQuestion "+c.moduleName)
				customQuestionHandlerList(c.moduleName).load(c)
			}
		}
		
	}
	
	def answerGiven(parm:ParamAnswerDefinition,result:Constant):Unit = lock.synchronized{
		if(dialogIsActive){
			System.out.println("Answer given "+parm+" "+result)
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
	}
	
	def processCustomEnquiry(resultList:Seq[(String,Constant)])= {
		isServerEnquiry=false
		ClientQueryManager.answerEnquiry(resultList)
	}
	
	def processResults = {
		val resultList=answerList.map(x =>(x.answer.name,x.result))	
		System.out.println("process Results "+resultList.mkString)
		if(isServerEnquiry){
			isServerEnquiry=false
			ClientQueryManager.answerEnquiry(resultList)
		}else 
		if(actionGroups!=null) {
			if(createType==0) // modification action
			for(group <-actionGroups)	
		  ClientQueryManager.executeAction(group.parent,group.children,currentAction,resultList)
		  // propertyField set to 0 because simpleCreateActions are handled in ActionPanel
		  else for(group <-actionGroups)
		  	ClientQueryManager.executeCreateAction(group.children,createType,0,currentAction,resultList)
		}
					
	}
	
	def initCustomQuestionHandlers = if(SystemSettings()!=null){	
		customQuestionHandlerList("client.print.PrintQuestionHandler")=PrintQuestionHandler
		val settingsString=SystemSettings().getClientSetting("CustomQuestionHandlers")
		if(settingsString.trim.size>0) {
			val moduleNames=settingsString.split(',')
			println("ModuleNames "+moduleNames.mkString)
			for (m <-moduleNames) try {
				customQuestionHandlerList(m)= Class.forName(m).newInstance.asInstanceOf[CustomQuestionHandler]
			} catch {
				case e => System.err.println("trying to instantiate CustomQuestionHandler module:'"+m+"' \n");e.printStackTrace
			}
		}
	}
}