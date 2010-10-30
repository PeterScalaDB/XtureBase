/**
 * Author: Peter Started:28.07.2010
 */
package definition.typ


/** defines a property field in a class
 * 
 */
case class PropertyFieldDefinition(name:String,single:Boolean,allowedClass:Int,
	createChildDefs:Seq[CreateChildDefinition]=Seq.empty) {
	
	def toXML() = 	{
		<PropertyFieldDef  name= {name} single= {if(single) "1" else "0"} allowedClass= {allowedClass.toString} >
   {createChildDefs.map(_.toXML)} 
		</PropertyFieldDef>
	}
}

/** defines a createAction to create a certain child type in that propertyField
 * @actionName name of an createAction in the child classes' Module, or "" for simply creating an empty child instance 
 * 
 */
case class CreateChildDefinition(editorName:String,childClassName:String,actionName:String) {
	def toXML() = {
		<CreateChild childClass={childClassName} action={actionName} editor={editorName} />
	}
	private var childTypID=0
	private var action:AbstractAction=null
	
	def getChildTyp= {
		if(childTypID== 0) childTypID=AllClasses.get.getClassIDByName(childClassName)
		childTypID	
	}
	def getAction = {
		if(action==null) {
			val aclass=AllClasses.get.getClassByID(getChildTyp)
		  action=  if(aclass.createActions.contains(actionName)) aclass.createActions(actionName)
		  else throw new IllegalArgumentException("cant find Action ["+ actionName+"] in Class " +childClassName  )		  
		} 
		action
	}
}

object CreateChildDefinition {
	def fromXML(node: scala.xml.Node) = {
		CreateChildDefinition((node \ "@editor").text,(node \ "@childClass").text,(node \ "@action").text)
	}
}


object PropertyFieldDefinition {
	def fromXML(node: scala.xml.Node) = 	{
		val name= (node \ "@name").text
		val single= ((node \ "@single").text=="1")
		val allowedClass= (node \ "@allowedClass").text.toInt		
		val createChildList = for(afield <-(node \\"CreateChild")) yield CreateChildDefinition.fromXML(afield)
		PropertyFieldDefinition(name,single,allowedClass,createChildList)		
	}
}