/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import collection.mutable._
import scala.swing.Component

/** abstract definition of a content component that fits in a viewbox
 * 
 */
trait ViewboxContent {
	type CompType=ViewboxContent with Component
	def open()
	def close()
	def storeSettings()
	//def getType:ViewboxContentType
	//def title:String
	def setViewbox(box:Viewbox)
	def typeID:String
}


case class ViewboxContentType(val id:Int,val name:String,val buttonText:String,val factory:()=>ViewboxContent#CompType)

object ViewboxContentTypeList {	
	val list=ArrayBuffer[ViewboxContentType]()
	
	def addType(newType:ViewboxContentType)= {
		list +=newType
	}
	def size=list.size
}