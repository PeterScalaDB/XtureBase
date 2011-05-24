/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data._
import scala.collection.immutable.IndexedSeq
import client.comm.ClientSystemSettings
import definition.typ.form.FormBox


/**
 *  description of a Class
 */
class ClientObjectClass (val name:String,val id:Int,val description:String,protected val ownFields:Seq[FieldDefinition],
	protected val ownFieldSettings:Seq[FieldSetting],
	 protected val ownPropFields:Seq[PropertyFieldDefinition],protected val theActions:Seq[ActionDescription], 
	 protected val theCreateActions:Seq[ActionDescription],protected val superClasses:Seq[Int],
	 val shortFormat:InstFormat,val longFormat:InstFormat,val resultFormat:InstFormat,val formBox:Option[FormBox],
	 val customInstanceEditor:Option[String], val ownAutoCreateInfos:Seq[AutoCreateInfo]=Seq.empty)
	 extends AbstractObjectClass
{  
   //System.out.println("Class "+name+" actions:"+actions.mkString(","))
   
	 //if(name=="NGewerk")println("N Fields "+ownFields.mkString(",")+"\n settings:"+ownFieldSettings.mkString(","))
	  
   def ownActions=theActions.iterator
   def ownCreateActions=theCreateActions.iterator
   
   var enumFields:Seq[(Int,EnumDefinition)]= Seq.empty // position of enum fields
   
   override def resolveSuperFields()= {
  	 super.resolveSuperFields
  	 try {
  	 enumFields=for(i<-fields.indices;val f=fields(i);if (f.typ ==DataType.EnumTyp )) 
  		 yield(i,SystemSettings().enumByID(f.asInstanceOf[EnumFieldDefinition].enumID))
  	 } catch { case e => System.err.println(e);}
  	 //if(enumFields.size>0) println("class "+name+" enumFields:"+enumFields.mkString(","))
  	 //if(name=="NGewerk")println("resolved Fields "+ownFields.mkString(",")+"\n settings:"+ownFieldSettings.mkString(","))
   }
		
}

object ClientObjectClass 
{	
	def stringToIntList(text:String):Seq[Int]= 
		if(text==null || text.length==0) return Seq.empty
		else return text.split(",").map(_.toInt)
	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node) =
	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		val actionsNode=node \"Actions"
		val createActionsNode=node \"CreateActions"
		val superClasses=stringToIntList ((node \"@superC").text)
		val instEditorName=(node \"@edit").text
		var shortForm:InstFormat=null
		//if(name=="NGewerk") println((node \\"FieldSetting").mkString("\n"))
		new ClientObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
			for(afield <-(node \\"FieldSetting")) yield FieldSetting.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  for(efield <-(actionsNode \\"Action"))yield ActionDescription.fromXML(efield),
		  for(efield <-(createActionsNode \\"Action"))yield ActionDescription.fromXML(efield),
		  superClasses,{
		  	shortForm=InstFormat.read(node \"@shortForm")
		  	shortForm
		  }
		  ,{
		  	val lf=InstFormat.read(node \"@longForm")
		  	if(lf==NOFORMAT) shortForm else lf
		  },InstFormat.read(node \"@resForm"),
		  readFormBox(node),
		  if(instEditorName.size==0)None else Some(instEditorName),
		  for(afield <-(node \\"AutoCreate")) yield AutoCreateInfo.fromXML(afield)
		  )
		}
	
	def readFormBox(node:scala.xml.Node): Option[FormBox] = {
		val elList=for(abox <-(node \\"FormBox")) yield FormBox(abox)
		if(elList.isEmpty) None
		else Some(elList.first)
	}
	
}




