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
  
  def initPrinting(pWidth:Float,pHeight:Float,nform:FormDescription)= {
  	pageWidth=pWidth
  	pageHeight=pHeight
  	pagesList.clear
  	elementList.clear
  	form=nform
  	currentXPos=form.left
  	currentYPos=form.top
  }
  
  def addPage() = {
  	pagesList +=new PageData(pagesList.size+1,elementList)
  	elementList=collection.mutable.ArrayBuffer[PrintElement]()
  	currentXPos=form.left
  	currentYPos=form.top
  }
  
  def restHeight=pageHeight-currentYPos-form.bottom
  def restWidth=pageWidth-currentXPos -form.right
  
	def addStamp(stamp:Stamp,horDirection:Boolean)= {
  	//println("Add Stamp "+stamp+" h:"+horDirection+" currx:"+currentXPos+" curry:"+currentYPos)
		if(horDirection) {
			if(stamp.width>restWidth) addPage
			elementList++= stamp.getPrintElements(currentXPos,currentYPos,restWidth,restHeight)
			currentXPos+=stamp.minWidth
		}
		else {
			if(stamp.height>restHeight) addPage
			elementList++= stamp.getPrintElements(currentXPos,currentYPos,restWidth,restHeight)
			currentYPos+=stamp.minHeight
		}
	}
  
  def getPagesData = {
  	if (elementList.size>0) {
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