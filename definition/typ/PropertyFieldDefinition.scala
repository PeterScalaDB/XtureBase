/**
 * Author: Peter Started:28.07.2010
 */
package definition.typ

/** defines a property field in a class
 * 
 */
case class PropertyFieldDefinition(name:String,single:Boolean,allowedClass:Int) {
	
	def toXML() =
	{
		<PropertyFieldDef  name= {name} 
  	  	 single= {if(single) "1" else "0"}
  	    allowedClass= {allowedClass.toString} 
  	 />
	}	

}

object PropertyFieldDefinition
{
	def fromXML(node: scala.xml.Node) = 
	{
		val name= (node \ "@name").text
		val single= ((node \ "@single").text=="1")
		val allowedClass= (node \ "@allowedClass").text.toInt		
		PropertyFieldDefinition(name,single,allowedClass)		
	}
}