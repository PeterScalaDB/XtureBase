/**
 * Author: Peter Started:23.12.2010
 */
package client.print

import scala.swing._
import scala.swing.event._
import java.awt.event.{WindowAdapter,WindowEvent}
import javax.swing.BorderFactory
import javax.swing.border.TitledBorder
import client.dataviewer.{FieldColumnModel,MultilineEditor}
import definition.expression.{Expression,Constant,IntConstant,StringConstant,BoolConstant}
import definition.data.{FormDescription,Reference,OutputDefinition}
import client.dialog.DialogManager

/**
 * 
 */
class ChoseOutDefDialog(w:Window) extends Dialog(w) {
	title="Ausgabe-Definition wählen:"
		
  
	
	var outdefList:Seq[OutputDefinition]=Seq.empty
	//var formsList:Seq[FormDescription]=Seq.empty
	val cancelBut=new Button("Abbruch")
	val nextBut=new Button("Weiter ->")
	
	val createBut=new Button("Zusätzliche Definition Anlegen ...")
	val changeBut=new Button("Gewählte Definition ändern ...")
	val deleteBut=new Button("Gewählte Definition löschen")
	val outdefListView=new ListView[OutputDefinition](){
		selection.intervalMode=ListView.IntervalMode.Single		
	}
	
	private var changedODInst:Int = -1
	
	val outdefScroller=new ScrollPane {
		viewportView=outdefListView	
		border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Bereits definierte Ausgabe-Definitionen:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
	}	
	
	preferredSize=new Dimension(560,350)
	
	val mainPanel=new BorderPanel(){
		add(new BoxPanel(Orientation.Vertical ) {
			contents+=Swing.VStrut(10)+= outdefScroller+=Swing.VStrut(20)
		},BorderPanel.Position.Center)
		
		add(new BoxPanel(Orientation.Vertical){
			contents += new BoxPanel(Orientation.Vertical) {
				contents+=createBut+=changeBut+=deleteBut
			} += new BoxPanel(Orientation.Horizontal) {
				contents+=cancelBut+=Swing.HGlue+=nextBut
			}
				
		},BorderPanel.Position.South)
		listenTo(cancelBut,nextBut,createBut,changeBut,deleteBut)
		
		reactions+= {
			case ButtonClicked(`cancelBut`)=>close
			case ButtonClicked(`changeBut`) =>changeOutdef
			case ButtonClicked(`createBut`)=>createOutdef
			case ButtonClicked(`deleteBut`)=>deleteOutdef
			case ButtonClicked(`nextBut`)=>choseOutdef
		}
	}
	
	contents=mainPanel
	
	def getIx= if(outdefListView.selection.indices.isEmpty)-1 else outdefListView.selection.indices.first
	
	def changeOutdef =  if(!outdefListView.selection.indices.isEmpty){
		val selOD=outdefListView.selection.items.first
		changedODInst=selOD.odInst
		PrintQuestionHandler.newDialog.setLocationRelativeTo(createBut )
		PrintQuestionHandler.newDialog.showEditDialog(whenOutputDefChanged,selOD)
	}
	
	def createOutdef = {
		PrintQuestionHandler.newDialog.setLocationRelativeTo(createBut )
		PrintQuestionHandler.newDialog.showDialog(whenNewoutputDefined)
	}
	
	def deleteOutdef = if(!outdefListView.selection.indices.isEmpty){
		val selOD=outdefListView.selection.items.first
		
		DialogManager.processCustomEnquiry(IndexedSeq(("DeleteOutDef",IntConstant(selOD.odInst)) ))
		outdefList=outdefList.filterNot(_.odInst ==selOD.odInst)
		println("filtered:"+outdefList)
		outdefListView.listData=outdefList
	}
	
	def choseOutdef = if(!outdefListView.selection.indices.isEmpty){
		val selOD=outdefListView.selection.items.first
		PrintQuestionHandler.newDialog.loadOutDefSettings(selOD)
		val sm=PrintQuestionHandler.newDialog.lastSelectedMedia
		DialogManager.processCustomEnquiry(IndexedSeq(("ChoseOutDef",IntConstant(selOD.odInst)),
				("PageWidth",IntConstant(sm.width.toInt)),("PageHeight",IntConstant(sm.height .toInt)) ) )
		close		
	}
	
	
	def loadOutdefs(newList:Seq[OutputDefinition])= {		
		outdefList=newList
		outdefListView.listData=newList
		outdefListView.selection.indices+=0
	}
	
	def whenNewoutputDefined(formIx:Int,printer:String,pageSetting:String,portrait:Boolean,w:Int,h:Int,paramData:Seq[(String,Constant)])= {
	  close
	  PrintQuestionHandler.outputDefined(formIx,printer,pageSetting,portrait,w,h,paramData)
	}
	
	def whenOutputDefChanged(formIx:Int,printer:String,pageSetting:String,portrait:Boolean,w:Int,h:Int,paramData:Seq[(String,Constant)])= {
	  close
	  DialogManager.processCustomEnquiry(IndexedSeq(("ChangeOutDef",IntConstant(changedODInst)),("Form",IntConstant(formIx)),
  		("Printer",StringConstant(printer)),("PageSettings",StringConstant(pageSetting)),
  		("Portrait",BoolConstant(portrait)),("PageWidth",IntConstant(w)),("PageHeight",IntConstant(h)) ) ++ paramData)
	}
}