/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import definition.data.{PageData,PrintElement,TextPrintElement,PrintElType,FormDescription}
import java.io.DataOutput
/**
 * 
 */
class DataEater {
  val pagesList=collection.mutable.ArrayBuffer[PageData]()  
  
  var form:FormDescription = _	
  var elementList=collection.mutable.ArrayBuffer[PrintElement]()
  
  var pageWidth:Float=_
  var pageHeight:Float=_
  
  var currentXPos:Float=0
  var currentYPos:Float=0
  
  var currentHeader:Option[Stamp]=_
  var currentFooter:Option[Stamp]=_
  
  var hasPrintElements:Boolean=false
  var currentContext:PrintContext=_
  
  def initPrinting(pWidth:Float,pHeight:Float,nform:FormDescription,ctx:PrintContext)= {
  	pageWidth=pWidth
  	pageHeight=pHeight
  	pagesList.clear
  	elementList.clear
  	form=nform
  	currentXPos=form.left
  	currentYPos=form.top
  	currentHeader=None
  	currentFooter=None
  	hasPrintElements=false
  	currentContext=ctx
  	ctx.setPageNr(1)
  }
  
  def addPage() = if(hasPrintElements){
  	pagesList +=new PageData(pagesList.size+1,elementList)  	
  	initPage() 	
  }
  
  def initSection() = {
  	if(hasPrintElements)addPage()
  	else initPage()
  }
  
  def initPage() = {
  	currentXPos=form.left
  	currentYPos=form.top
  	elementList=collection.mutable.ArrayBuffer[PrintElement]()
  	currentContext.setPageNr(pagesList.size+1)
  	for(c<-currentHeader) {
  		c.updateVariables(PrintGenerator.context)
  		elementList++= c.getPrintElements(currentXPos,currentYPos,restWidth,c.minHeight)  
  		currentYPos+=c.minHeight
  	}
  	for(c<-currentFooter) {
  		c.updateVariables(PrintGenerator.context)
  		elementList++= c.getPrintElements(currentXPos,pageHeight-form.bottom-c.minHeight,restWidth,c.minHeight)
  	}
  	hasPrintElements=false
  }
  
  def restHeight=pageHeight-currentYPos-form.bottom-(currentFooter match {
  	case Some(f)=>f.minHeight
  	case _=> 0})
  def restWidth=pageWidth-currentXPos -form.right
  
	def addStamp(stamp:Stamp,horDirection:Boolean)= {
  	//println("Add Stamp "+" h:"+horDirection+" currx:"+currentXPos+" curry:"+currentYPos+" restHeight:"+restHeight)
  	if(!hasPrintElements)hasPrintElements=true
		if(horDirection) {
			if(stamp.minWidth>restWidth) addPage
			elementList++= stamp.getPrintElements(currentXPos,currentYPos,restWidth,restHeight)
			currentXPos+=stamp.minWidth
		}
		else {
			if(stamp.minHeight>restHeight) addPage
			elementList++= stamp.getPrintElements(currentXPos,currentYPos,restWidth,restHeight)
			currentYPos+=stamp.minHeight
		}
	}
  
  def getPagesData = {
  	if (hasPrintElements) {
  	  addPage	
  	}
  	pagesList
  }
  
  
  def write(out:DataOutput)= {
  	getPagesData  	
  	out.writeInt(pagesList.size)
  	pagesList.foreach(_.write(out))
  }
  //override def toString= pagesList.mkString("\n")
}