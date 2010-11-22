/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data._
import definition.expression.EMPTY_EX
import scala.collection.immutable.IndexedSeq


/**
 *  description of a Class
 */
class ClientObjectClass (val name:String,val id:Int,val description:String,protected val ownFields:Seq[FieldDefinition],
	protected val ownFieldSettings:Seq[FieldSetting],
	 protected val ownPropFields:Seq[PropertyFieldDefinition],protected val theActions:Seq[ActionDescription], 
	 protected val theCreateActions:Seq[ActionDescription],protected val superClasses:Seq[String],
	 val shortFormat:InstFormat,val longFormat:InstFormat,val resultFormat:InstFormat)
	 extends AbstractObjectClass
{  
   //println("Class "+name+" actions:"+actions.mkString(","))
   
   def ownActions=theActions.iterator
   def ownCreateActions=theCreateActions.iterator
   /*if(theCreateActions.size>0) {
  	 println()
  	 println("***** class :"+name+" createActions:"+theCreateActions.mkString)
   }*/
  
  //TODO: check class Version when testing inheritance. InstanceProperties needs to store the class versions
  	
		
}

object ClientObjectClass 
{	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node) =
	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		val actionsNode=node \"Actions"
		val createActionsNode=node \"CreateActions"
		var shortForm:InstFormat=null
		new ClientObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
			for(afield <-(node \\"FieldSetting")) yield FieldSetting.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  for(efield <-(actionsNode \\"Action"))yield ActionDescription.fromXML(efield),
		  for(efield <-(createActionsNode \\"Action"))yield ActionDescription.fromXML(efield),
		  for(cfield <-(node \\ "sc"))  yield (cfield \ "@name").text,{
		  	shortForm=InstFormat.read(node \"@shortForm")
		  	shortForm
		  }
		  ,{
		  	val lf=InstFormat.read(node \"@longForm")
		  	if(lf==null) shortForm else lf
		  },InstFormat.read(node \"@resForm"))	}
	
	
}




