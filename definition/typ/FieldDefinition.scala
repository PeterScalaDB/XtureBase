package definition.typ

/**
 * Author: Peter Started:26.06.2010
 */

/**
 * Description of a single Datafield in a DataObject
 */
class FieldDefinition (val name:String,val typ: DataType.Value) 
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
   
   def setType(newType:DataType.Value)= 
  	 if(newType==DataType.EnumTyp.id) new EnumFieldDefinition(name,0)
  	 else new FieldDefinition(name,newType)
   
   def setEnumID(newID:Int)= {
  		 //println("set enum ID:"+newID)
  	 if(typ==DataType.EnumTyp) new EnumFieldDefinition(name,newID)
  	 else this
   } 
   
   override def equals(other: Any) = other match {
		case that: FieldDefinition => that.canEqual(this) && this.name == that.name && this.typ==that.typ 
		case _ => false
	}
	override def hashCode = 41 * name.hashCode + 1041*typ.hashCode+3
	
	def canEqual(that: FieldDefinition) = true   
}

case class EnumFieldDefinition(nname:String,val enumID:Int) extends FieldDefinition(nname,DataType.EnumTyp) {
	override def toXML = 
   {  	 
  	 <FieldDef  name={name} typ={typ.id.toString} enumID={enumID.toString}
  	 /> 
   } 
  override def toString = "E"+super.toString+" eid:"+enumID
}



object FieldDefinition
{
	def fromXML(node: scala.xml.Node) = 
	{
		val name= (node \ "@name").text
		val typ= (node \ "@typ").text.toInt	
	  if(typ==DataType.EnumTyp.id)  EnumFieldDefinition(name,(node \ "@enumID").text.toInt)	
		else new FieldDefinition(name,DataType(typ))
		
	}
}