/**
 * Author: Peter Started:28.07.2010
 */
package definition.typ


/** defines a property field in a class
 * 
 */
case class PropertyFieldDefinition(name:String,single:Boolean=false,allowedClass:Int=0,
	createChildDefs:Seq[CreateChildDefinition]=Seq.empty) {
	
	def toXML() = 	{
		<PropertyFieldDef  name= {name} single= {if(single) "1" else "0"} allowedClass= {allowedClass.toString} >
   {createChildDefs.map(_.toXML)} 
		</PropertyFieldDef>
	}
	
	def setName(newName:String)=new PropertyFieldDefinition(newName,single,allowedClass,createChildDefs)
	def setSingle(newValue:Boolean)=new PropertyFieldDefinition(name,newValue,allowedClass,createChildDefs)
	def setAllowedClass(newValue:Int)=new PropertyFieldDefinition(name,single,newValue,createChildDefs)
	def setChildDefs(newList:Seq[CreateChildDefinition])= new PropertyFieldDefinition(name,single,allowedClass,newList)
}

/** defines a createAction to create a certain child type in that propertyField
 * @actionName name of an createAction in the child classes' Module, or "" for simply creating an empty child instance 
 * 
 */
case class CreateChildDefinition(editorName:String="",childClassID:Int=0,actionName:String="") {
	def toXML() = {
		<CreateChild childClassID={childClassID.toString} action={actionName} editor={editorName} />
	}
	lazy val childName=AllClasses.get.getClassByID(childClassID).name
	lazy val action = {
		val aclass=AllClasses.get.getClassByID(childClassID)
	  if(aclass.createActions.contains(actionName)) aclass.createActions(actionName)
	  else throw new IllegalArgumentException("cant find Action ["+ actionName+"] in Class " +childClassID  )		  
	}
	def setEditorName(newValue:String) = new CreateChildDefinition(newValue,childClassID,actionName)
	def setChildClass(newValue:Int) = new CreateChildDefinition(editorName,newValue,actionName)
	def setActionName(newValue:String) = new CreateChildDefinition(editorName,childClassID,newValue)
	
	
}

object CreateChildDefinition {
	def fromXML(node: scala.xml.Node) = {
		CreateChildDefinition((node \ "@editor").text,(node \ "@childClassID").text.toInt,(node \ "@action").text)
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