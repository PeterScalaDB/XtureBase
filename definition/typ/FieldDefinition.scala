package definition.typ

/**
 * Author: Peter Started:26.06.2010
 */

/**
 * Description of a single Datafield in a DataObject
 */
case class FieldDefinition (name:String,typ: DataType.Value,readOnly:Boolean=false,
	visible:Boolean=true,showFormula:Boolean=false) 
{
   override def toString = 
   { 
  	 "Field["+name+":"+typ+" "+(if(readOnly)"ReadOnly"else"Writeable")+" "+(if(visible)"Visible"else"Hidden")+
  	 (if(!showFormula)" showResult ")+"]"   
   }
   
   /** 
   * converts to XML
   */
   def toXML = 
   {  	 
  	 <FieldDef  name={name} 
  	 typ={typ.id.toString}
  	 readonly={if(readOnly)"1" else "0"}
  	 visible={if(visible)"1" else "0"}
  	 showForm={if(showFormula)"1" else "0"}
  	 /> 
   }   
}

object FieldDefinition
{
	def fromXML(node: scala.xml.Node) = 
	{
		val name= (node \ "@name").text
		val typ= (node \ "@typ").text.toInt
		val readonly= ((node \ "@readonly").text=="1")
		val visible= (node \ "@visible").text!="0"
		val showForm= (node \ "@showForm").text!="0"
		FieldDefinition(name,DataType(typ),readonly,visible,showForm)
		
	}
}