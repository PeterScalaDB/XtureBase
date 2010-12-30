/**
 * Author: Peter Started:21.12.2010
 */
package client.print

import client.dialog.{CustomQuestionHandler,DialogManager}
import definition.typ.CustomQuestion
import definition.data.{Reference,InstanceData,FormDescription,OutputDefinition,PageData,FontStyleList}
import definition.expression.{StringConstant,Constant,IntConstant,BoolConstant}
import client.testUI.ViewTest
import client.comm.{GenDataReceiver}
import java.io.DataInput

/**
 * 
 */
object PrintQuestionHandler extends CustomQuestionHandler with GenDataReceiver{
  val newDialog= new NewOutdefDialog(ViewTest.top)
  val outdefDialog=new ChoseOutDefDialog(ViewTest.top)
  var forms:Seq[FormDescription]=Seq.empty
  var outDefs:Seq[OutputDefinition]=Seq.empty
  
  val myPageable=new MyPageable
  newDialog.printJob .setPageable(myPageable)
  
  var choosenOutDef:OutputDefinition= _
  
   
  def readInXML(xmlData:Seq[scala.xml.Node]) = {
    forms=for (f<-(xmlData \\"Form")) yield FormDescription.fromXML(f)
  	outDefs=for (f<-(xmlData \\"OutDef")) yield OutputDefinition.fromXML(f) 	
  	
		for(od<-outDefs;val formID=od.formInst )
			 forms.find(_.inst==formID) match {
			case Some(form)=> od.formName=form.name
			case None => System.err.println("Cant find form with id:"+formID)
		}
	
  	
  	println("Forms: "+forms.mkString("\n"))
  	println("OutDefs: "+outDefs.mkString("\n"))	
  }
  
  def load(question: CustomQuestion): Unit = {
  	readInXML(question.customData)
  	newDialog.loadForms( forms)
  	
  	if(outDefs.isEmpty) {
  		
  		newDialog.setLocationRelativeTo(ViewTest.actionPan )
  		newDialog.showDialog(outputDefined)
  	}
  	else {
  		outdefDialog.loadOutdefs(outDefs)
  		outdefDialog.setLocationRelativeTo(ViewTest.actionPan)
  		outdefDialog.visible=true
  	}
  }
  
  def outputDefined(formIx:Int,printer:String,pageSetting:String,portrait:Boolean,w:Int,h:Int,paramData:Seq[(String,Constant)])= {  	
  	DialogManager.processCustomEnquiry(IndexedSeq(("NewOutDef",IntConstant(formIx)),
  		("Printer",StringConstant(printer)),("PageSettings",StringConstant(pageSetting)),
  		("Portrait",BoolConstant(portrait)),("PageWidth",IntConstant(w)),("PageHeight",IntConstant(h))) ++ paramData)
  }
  
  def showPreviewWindow(xmlData:Seq[scala.xml.Node],usedOutDef:Int,pageData:Seq[PageData]) = {
  	readInXML(xmlData)
  }
  
  def receiveData(in:DataInput)= {
  	val form=newDialog.getCurrentForm
  	println(form.fonts.list .mkString(","))  	
  	val jobTitle=in.readUTF
  	val pagesList=for( i <-0 until in.readInt) yield PageData(in)  	
  	val printableArea=newDialog.getPrintableArea
  	val pageFormat=newDialog.getPageFormat  	
  	println("Printable Area:"+printableArea)
  	println("Page data:\n"+pagesList.mkString("\n"))
  	println("PageFormat:"+pageFormat.getImageableWidth+ " x "+pageFormat.getImageableHeight)
  	
  	MyContext.fontStyleList=form.fonts
  	
  	myPageable.setData(pageFormat,pagesList)  	
  	newDialog.print(jobTitle)
  }

}