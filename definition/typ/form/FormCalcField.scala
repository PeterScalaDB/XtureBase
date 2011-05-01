/**
 * Author: Peter Started:21.04.2011
 */
package definition.typ.form

import scala.swing.Label
import scala.xml.Node
import definition.data.InstanceData
import definition.typ.HorAlign
import definition.expression.Expression
import definition.expression.FieldReference
import definition.typ.DataType

/**
 * 
 */
class FormCalcField(val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val align:HorAlign.Value,val expression:Expression) extends Label with FormDataField {  

	val fieldNr:Byte=0.toByte
	val refList:List[FieldReference]=expression.getElementList[FieldReference](DataType.FieldRefTyp,Nil)
	
	setupComponent(this)
	
	//println("Calc init "+expression.getTerm+" "+expression.getValue)
	text=expression.getValue.toString
	
	horizontalAlignment=HorAlign.toScalaAlignment(align)
	
  def wantShutDown(): Unit = {  }

  def shutDown(): Unit = {  }

  def setDataValue(dvalue: InstanceData): Unit = {  
  	for(r<-refList) {
  		r.cachedValue =dvalue.fieldValue (r.remField )  		
  	}
  	text=expression.getValue.toString
  }
  
  def makeCopy = new FormCalcField(minWidth,maxWidth,minHeight,maxHeight,align,expression.createCopy)
  
  def toXML() = <CalcField minWidth={minWidth.toString} maxWidth={maxWidth.toString} minHeight={minHeight.toString} maxHeight={maxHeight.toString} align={align.id.toString} ex={expression.getTerm}/>

}