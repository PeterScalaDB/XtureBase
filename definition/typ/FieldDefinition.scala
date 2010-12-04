package definition.typ

/**
 * Author: Peter Started:26.06.2010
 */

/**
 * Description of a single Datafield in a DataObject
 */
case class FieldDefinition (val name:String,val typ: DataType.Value) 
{
   override def toString = 
   { 
  	 "Field["+name+":"+typ+"]"   
   }
   
   /** 
   * converts to XML
   */
   def toXML = 
   {  	 
  	 <FieldDef  name={name} 
  	 typ={typ.id.toString}  	 
  	 /> 
   }  
   
   def setName(newName:String)=new FieldDefinition(newName,typ)
   def setType(newType:DataType.Value)=new FieldDefinition(name,newType)
    
}

object FieldDefinition
{
	def fromXML(node: scala.xml.Node) = 
	{
		val name= (node \ "@name").text
		val typ= (node \ "@typ").text.toInt	
		
		FieldDefinition(name,DataType(typ))
		
	}
}