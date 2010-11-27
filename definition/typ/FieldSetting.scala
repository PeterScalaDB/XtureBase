/**
 * Author: Peter Started:20.11.2010
 */
package definition.typ

import definition.expression._
/**
 * 
 */
case class FieldSetting (fieldNr:Int,readOnly:Boolean=false,
	visible:Boolean=true,showFormula:Boolean=false,editor:String="",startValue:Expression,formString:String="") { 
  
	override def toString = 
   { 
  	 "FieldSetting("+fieldNr+") "+(if(readOnly)"ReadOnly"else"Writeable")+" "+(if(visible)"Visible"else"Hidden")+
  	 (if(showFormula)" showFormula ")+(if(editor.size>0)" editor:"+editor)+" startValue:"+startValue+"formString:"+formString+"]"   
   }
	
	def toXML = 
   {  	 
  	 <FieldSetting  fieldNr={fieldNr.toString}
  	 readonly={if(readOnly)"1" else "0"}
  	 visible={if(visible)"1" else "0"}
  	 showForm={(if(showFormula)"1" else "0")}
  	 ed={editor }
  	 startValue={startValue.getTerm}
  	 format={formString}
  	 /> 
   } 
	
}

object EmptySetting extends FieldSetting(0,false,true,false,"",EMPTY_EX)

object FieldSetting
{
	def fromXML(node: scala.xml.Node) = 
	{
		val fnr= (node \ "@fieldNr").text.toInt
		val readonly= (node \ "@readonly").text=="1"
		val visible= (node \ "@visible").text=="1"
		val showForm= (node \ "@showForm").text=="1"
		val editor= (node \ "@ed").text
		val startV=(node \ "@startValue").text
		val startEx=if(startV=="") EMPTY_EX else StringParser.parse(startV)
		val format=(node \ "@format").text
		//if(editor.size>0) println(" edit:"+name)
		
		FieldSetting(fnr,readonly,visible,showForm,editor,startEx,format)
		
	}
}