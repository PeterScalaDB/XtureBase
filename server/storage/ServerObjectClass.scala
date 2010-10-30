/**
 * Author: Peter Started:21.09.2010
 */
package server.storage


import definition.typ._
import definition.data._
import definition.expression._
import scala.collection.immutable.IndexedSeq
/**
 * 
 */
class ServerObjectClass (val name:String,val id:Int,val description:String,protected val ownFields:Seq[FieldDefinition],
	 protected val ownPropFields:Seq[PropertyFieldDefinition],protected val theActions:Seq[AbstractAction], 
	 protected val theCreateActions:Seq[AbstractAction],protected val superClasses:Seq[String],
	 moduleName:String)
	 extends AbstractObjectClass {
	
	def ownActions = theActions.iterator
	def ownCreateActions = theCreateActions.iterator
	
	def toXML() =
	{
		//if(theCreateActions.size>0) println("class "+name+" DUMP createActions:"+theCreateActions.map(a =>a.toXML))
		val sc=superClasses.map { a =>  <sc name= { a }  />  }		
		<ObjectClass name={name} id={id.toString} desc={description}>
		<Fields> 		{			ownFields.map(i => i.toXML)	}</Fields>
		<PropFields> 		{			ownPropFields.map(i => i.toXML)	}</PropFields>
		<SuperClasses> {  sc }</SuperClasses>
		<Actions> {theActions.map(a => a.toXML)} </Actions>
    <CreateActions> {theCreateActions.map(a => a.toXML)} </CreateActions>
		</ObjectClass>
	}	
	
	
	/** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference]):InstanceData =
  {
  	new InstanceData(ref,Array.fill(fields.size)(EMPTY_EX),owner,false)
  }
  
  /** creates an empty InstanceProperty of this class
   * 
   * @param ref the instance of that Property list
   * @return the new Property object
   */
  def createInstanceProperty(ref:Reference):InstanceProperties =
  {
  	val pArray=(for(i <- 0 until propFields.size) 
  		yield new PropertyFieldData(propFields(i).single,IndexedSeq.empty)).toArray
  	new InstanceProperties(ref,pArray)
  }

}


object ServerObjectClass 
{	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node):ServerObjectClass =
	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		val moduleName=(node \"@moduleName").text
		var actionList:Seq[AbstractAction]= null
		var createActionList:Seq[AbstractAction]= null
		
		if(moduleName==""){
		  actionList=IndexedSeq.empty
		  createActionList=IndexedSeq.empty
		} else {
			val module=ActionModule.load(moduleName)
			actionList=module.getActionsIterator.toSeq
			createActionList=module.getCreateActionsIterator.toSeq
			//if(createActionList.size>0) println("class:"+name+" Module:"+moduleName+" "+createActionList.mkString)
		}
		
		 
			
		new ServerObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  actionList,createActionList,
		  for(cfield <-(node \\ "sc"))  yield (cfield \ "@name").text,
		  moduleName)
	}
}