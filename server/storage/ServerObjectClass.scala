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
class ServerObjectClass (val name:String,val id:Int,val description:String,protected val fields:Seq[FieldDefinition],
	 protected val propFields:Seq[PropertyFieldDefinition],protected val actions:Seq[ActionImpl], protected val superClasses:Seq[String],
	 moduleName:String)
	 extends AbstractObjectClass {
	
	
	def toXML() =
	{
		val sc=superClasses.map { a =>  <sc name= { a }  /> }		
		<ObjectClass name={name} id={id.toString} desc={description}>
		<Fields> 		{			fields.map(i => i.toXML)	}</Fields>
		<PropFields> 		{			propFields.map(i => i.toXML)	}</PropFields>
		<SuperClasses> {  sc }</SuperClasses>
		<Actions> {actions.map(a => a.toXML)} </Actions>
		</ObjectClass>
	}	
	
	
	/** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference]):InstanceData =
  {
  	new InstanceData(ref,Array.fill(getFieldCount)(EMPTY_EX),owner,false)
  }
  
  /** creates an empty InstanceProperty of this class
   * 
   * @param ref the instance of that Property list
   * @return the new Property object
   */
  def createInstanceProperty(ref:Reference):InstanceProperties =
  {
  	val pArray=(for(i <- 0 until getPropFieldCount) 
  		yield new PropertyFieldData(propField(i).single,IndexedSeq.empty)).toArray
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
		val actionList:Seq[ActionImpl] = if(moduleName=="") IndexedSeq.empty
			else {
				val module=ActionModule.load(moduleName)
				val count=module.getActionCount
				for(i <- 0 until count) yield module.getAction(i)
		  }
		
		new ServerObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  actionList,
		  for(cfield <-(node \\ "sc"))  yield (cfield \ "@name").text,
		  moduleName)
	}
}