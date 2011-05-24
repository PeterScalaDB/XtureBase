/**
 * Author: Peter Started:21.09.2010
 */
package server.storage


import definition.typ._
import definition.data._
import definition.expression._
import scala.collection.immutable.IndexedSeq
import transaction.handling.TransactionManager
import definition.typ.form.FormBox
/**
 * 
 */
class ServerObjectClass (var name:String,var id:Int,var description:String="",var ownFields:Seq[FieldDefinition]=Seq.empty,
	  var ownFieldSettings:Seq[FieldSetting]=Seq.empty,
	  var ownPropFields:Seq[PropertyFieldDefinition]=Seq.empty,protected val theActions:Seq[AbstractAction]=Seq.empty, 
	 protected val theCreateActions:Seq[AbstractAction]=Seq.empty,var superClasses:Seq[Int]=Seq.empty,
	 var moduleName:String="",var shortFormat:InstFormat=NOFORMAT,var longFormat:InstFormat=NOFORMAT,var resultFormat:InstFormat=NOFORMAT,
	 val formBox:Option[FormBox]=None,var customInstanceEditor:Option[String]=None,
	 val ownAutoCreateInfos:Seq[AutoCreateInfo]=Seq.empty)
	 extends AbstractObjectClass {
	
	def ownActions = theActions.iterator
	def ownCreateActions = theCreateActions.iterator
	
	def toXML() = 	{			
		//if(name=="NGewerk") println("toXML "+ownFieldSettings.mkString("\n"))
		<ObjectClass name={name} id={id.toString} desc={description} superC={superClasses.mkString(",")} edit={
			customInstanceEditor match {case Some(e)=>e;case _=>""}}  shortForm={shortFormat.toString} 
			longForm={longFormat.toString} resForm={resultFormat.toString} >
		<Fields> 		{			ownFields.map(_.toXML)	}</Fields>
    <FieldSettings>{ownFieldSettings.map(_.toXML)}</FieldSettings>
		<PropFields> 		{			ownPropFields.map(_.toXML)	}</PropFields>		
		<Actions> {theActions.map(_.toXML)} </Actions>
    <CreateActions> {theCreateActions.map(_.toXML)} </CreateActions>
    <Forms>{formBox match {case Some(b)=> b.toXML ;case _ => xml.Null} }</Forms>
    <AC> {ownAutoCreateInfos.map(_.toXML)}</AC>
		</ObjectClass>
	}	
	
	def saveToXML() = {
		//if(name=="NGewerk") println("saveToXML "+ownFields.mkString("\n"))
		<ObjectClass name={name} id={id.toString} desc={description} superC={superClasses.mkString(",")} edit={
			customInstanceEditor match {case Some(e)=>e;case _=>""}}     shortForm={shortFormat.toString} 
			longForm={longFormat.toString} resForm={resultFormat.toString} moduleName={moduleName}>
		<Fields> 		{			ownFields.map(_.toXML)	}</Fields>
    <FieldSettings>{ownFieldSettings.map(_.toXML)}</FieldSettings>
		<PropFields> 		{			ownPropFields.map(_.toXML)	}</PropFields>		
		<Actions> {theActions.map(_.toXML)} </Actions>
    <CreateActions> {theCreateActions.map(_.toXML)} </CreateActions>
    <Forms>{formBox match {case Some(b)=> b.toXML ;case _ => xml.Null}}</Forms>
    <AC> {ownAutoCreateInfos.map(_.toXML)}</AC>
		</ObjectClass>
	}
	
	def makeClone= {
		val theClone=new ServerObjectClass(name,id,description,ownFields,ownFieldSettings,ownPropFields,theActions,theCreateActions,
		superClasses,moduleName,shortFormat,longFormat,resultFormat,formBox,customInstanceEditor,ownAutoCreateInfos)
		theClone.resolveSuperFields
		theClone
	}
	
	def setFormBox(newValue:Option[FormBox])= {
		val theClone=new ServerObjectClass(name,id,description,ownFields,ownFieldSettings,ownPropFields,theActions,theCreateActions,
		superClasses,moduleName,shortFormat,longFormat,resultFormat,newValue,customInstanceEditor,ownAutoCreateInfos)
		theClone.resolveSuperFields
		theClone		
	}
	
	def setAutoCreateInfo(aci:Seq[AutoCreateInfo])= {
		val theClone=new ServerObjectClass(name,id,description,ownFields,ownFieldSettings,ownPropFields,theActions,theCreateActions,
		superClasses,moduleName,shortFormat,longFormat,resultFormat,formBox,customInstanceEditor,aci)
		theClone.resolveSuperFields
		theClone
	}
	
	
	
	
	/** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference],withStartValues:Boolean):InstanceData =
  {
  	val fieldExpressions:collection.IndexedSeq[Expression]=
  		for(i <-fields.indices) yield 
  			if(withStartValues) {
  				val sv=fieldSetting(i).startValue
  				if (sv.isNullConstant) Expression.generateNullConstant(fields(i).typ )
  				else fieldSetting(i).startValue.generate  		
  			}
  			else Expression.generateNullConstant(fields(i).typ ) 
  	new InstanceData(ref,fieldExpressions,owner,Seq.empty,false)
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
  
  def getNumOwnFields=ownFields.size
  
  def getNumOwnPropFields=ownPropFields.size

}



object EmptyServerClass  extends ServerObjectClass ("",0)


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
		val superClasses=ClientObjectClass.stringToIntList((node \"@superC").text)
		val instEditorName=(node \"@edit").text
		if(moduleName==""){
		  actionList=IndexedSeq.empty
		  createActionList=IndexedSeq.empty
		} else {
			val module=ActionModule.load(moduleName)
			module.setObjectType(id)
			actionList=module.getActionsIterator.toSeq
			createActionList=module.getCreateActionsIterator.toSeq
			//if(createActionList.size>0) System.out.println("class:"+name+" Module:"+moduleName+" "+createActionList.mkString)
		}			
		new ServerObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
			for(afield <-(node \\"FieldSetting")) yield FieldSetting.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  actionList,createActionList,
		  superClasses,
		  moduleName,InstFormat.read(node \"@shortForm"),InstFormat.read(node \"@longForm"),
		  InstFormat.read(node \"@resForm"),readFormBox(node),
		  if(instEditorName.size==0)None else Some(instEditorName),
		  for(afield <-(node \\"AutoCreate")) yield AutoCreateInfo.fromXML(afield) )
	}
	
	def readFormBox(node:scala.xml.Node): Option[FormBox] = {
		val elList=for(abox <-(node \\"FormBox")) yield FormBox(abox)
		if(elList.isEmpty) None
		else Some(elList.first)
	}
}