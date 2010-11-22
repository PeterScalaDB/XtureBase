/**
 * Author: Peter Started:21.09.2010
 */
package server.storage


import definition.typ._
import definition.data._
import definition.expression._
import scala.collection.immutable.IndexedSeq
import transaction.handling.TransactionManager
/**
 * 
 */
class ServerObjectClass (val name:String,val id:Int,val description:String,protected val ownFields:Seq[FieldDefinition],
	 protected val ownFieldSettings:Seq[FieldSetting],
	 protected val ownPropFields:Seq[PropertyFieldDefinition],protected val theActions:Seq[AbstractAction], 
	 protected val theCreateActions:Seq[AbstractAction],protected val superClasses:Seq[String],
	 moduleName:String,val shortFormat:InstFormat,val longFormat:InstFormat,val resultFormat:InstFormat)
	 extends AbstractObjectClass {
	
	def ownActions = theActions.iterator
	def ownCreateActions = theCreateActions.iterator
	
	def toXML() =
	{
		//if(theCreateActions.size>0) println("class "+name+" DUMP createActions:"+theCreateActions.map(a =>a.toXML))
		val sc=superClasses.map { a =>  <sc name= { a }  />  }		
		<ObjectClass name={name} id={id.toString} desc={description} shortForm={shortFormat.toString}
		longForm={longFormat.toString} resForm={resultFormat.toString}>
		<Fields> 		{			ownFields.map(_.toXML)	}</Fields>
    <FieldSettings>{ownFieldSettings.map(_.toXML)}</FieldSettings>
		<PropFields> 		{			ownPropFields.map(_.toXML)	}</PropFields>
		<SuperClasses> {  sc }</SuperClasses>
		<Actions> {theActions.map(_.toXML)} </Actions>
    <CreateActions> {theCreateActions.map(_.toXML)} </CreateActions>
		</ObjectClass>
	}	
	
	
	/** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference],withStartValues:Boolean):InstanceData =
  {
  	val fieldExpressions:collection.IndexedSeq[Expression]=if(withStartValues)
  		for(i <-fields.indices) yield {
  			fieldSetting(i).startValue.generate  		
  		} else IndexedSeq.fill(fields.size)(getEmpty)
  	new InstanceData(ref,fieldExpressions,owner,false)
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
  
  def getEmpty=EMPTY_EX

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
			for(afield <-(node \\"FieldSetting")) yield FieldSetting.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  actionList,createActionList,
		  for(cfield <-(node \\ "sc"))  yield (cfield \ "@name").text,
		  moduleName,InstFormat.read(node \"@shortForm"),InstFormat.read(node \"@longForm"),
		  InstFormat.read(node \"@resForm"))
	}
}