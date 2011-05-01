/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ.form

import scala.swing.Label
import scala.xml.Node
import definition.data.InstanceData
import definition.typ.HorAlign

/**
 * 
 */
class FormLabel(val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val atext:String,val align:HorAlign.Value) extends Label with FormElement {
	
	text=atext
	horizontalAlignment=HorAlign.toScalaAlignment(align)
	setupComponent(this)
	
	 def toXML = 
   {  	 
  	 <FormLabel  minWidth={minWidth.toString} maxWidth={maxWidth.toString} minHeight={minHeight.toString} maxHeight={maxHeight.toString} align={align.id.toString}  	   
  	 text={atext}/> 
   } 
	
	override def toString= "Label "+atext
	
	def makeCopy = new FormLabel(minWidth,maxWidth,minHeight,maxHeight,atext,align)
	
	//override def getProperties= ("Text",atext)::("Align",align.toString)::super.getProperties  
}




