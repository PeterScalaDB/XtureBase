/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import definition.data._
import collection.mutable.{HashMap,ArrayBuffer,LinkedHashMap,LinkedHashSet}


/**
 * 
 */
trait AbstractObjectClass {
	
	def name:String
	
	def description:String
	
	def id:Int
	
	protected def ownFields:Seq[FieldDefinition]
	protected def ownPropFields:Seq[PropertyFieldDefinition] 
	protected def ownActions:Iterator[AbstractAction]
	protected def ownCreateActions:Iterator[AbstractAction]
	protected def superClasses:Seq[String]	
	protected def ownFieldSettings:Seq[FieldSetting]
	
	private var hasResolved=false	
	val fields=new ArrayBuffer[FieldDefinition]() // list of inherited fields from the super class
	val propFields = new ArrayBuffer[PropertyFieldDefinition]()
	val superClassIDs:LinkedHashSet[Int] = LinkedHashSet()
	val actions=LinkedHashMap[String,AbstractAction]()
	val createActions=LinkedHashMap[String,AbstractAction]()
	val fieldEditors=LinkedHashSet[String]()
	private val fieldSettingSet=LinkedHashMap[Int,FieldSetting]()
	def shortFormat:InstFormat
	def longFormat:InstFormat
	def resultFormat:InstFormat
		
	def inheritsFrom(otherClassID:Int):Boolean =
  {
  	//println( " " +toString +" "+id+"InheritsFrom: "+ otherClassID)
  	superClassIDs.contains(otherClassID)
  }
	
	def resolveSuperFields():Unit = 	{		
		if(!hasResolved)
		{	
			for(cl <-superClasses)
			{
				val superClass:AbstractObjectClass= AllClasses.get.getClassByName(cl) match {
					case Some(a) => a
					case None=> throw new IllegalArgumentException("Superclass "+cl+" is not defined, in class "+name)
				}
				superClass.resolveSuperFields()				
				superClassIDs ++=superClass.superClassIDs							
				copySuperClassFields(superClass)				
				propFields ++= superClass.propFields 
				actions ++=superClass.actions
				createActions ++=superClass.createActions
				fieldEditors ++=superClass.fieldEditors
			}	
			superClassIDs += id	
			// add own fields
			for(a <- ownFields) {
					var ix= fields.findIndexOf(a.name==_.name)
					if(ix<0){					// not found						
						fields += a
					}
					else System.err.println("can't redefine field "+a +" in class "+name)
			}		
			for(s <-ownFieldSettings) {
					if(s.editor.length>0)// editor defined
						fieldEditors +=s.editor
					fieldSettingSet(s.fieldNr)=s	
				}
			propFields ++=ownPropFields
			ownActions.foreach(a => actions(a.name)=a)	
			ownCreateActions.foreach(a => createActions(a.name)=a)
		  hasResolved=true
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}
	
	def copySuperClassFields(superClass:AbstractObjectClass) = {
		var fieldNum = fields.size
		fields++=superClass.fields
		for(i <- superClass.fields.indices)
		  if(superClass.fieldSettingSet.contains(i))
		  	fieldSettingSet(fieldNum+i)= superClass.fieldSettingSet(i)
	}
	
	def fieldSetting(fieldNr:Int)= {
	if(fieldSettingSet.contains(fieldNr) ) fieldSettingSet(fieldNr)
	else EmptySetting
}
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString(",")+"\nPropFields:\n"+propFields.mkString("\n")+
	"\nSuperclasses:"+ superClassIDs.mkString(",");
}



case class InstFormat(val formStr:String,val fields:Array[Int]){
	override def toString= formStr+"|"+fields.mkString(",")
}
object NOFORMAT extends InstFormat("",Array.empty) {
	override def toString=""
}

object InstFormat {
	def read(node: scala.xml.NodeSeq) = {
		val tx=node.text
		if(tx=="") NOFORMAT
		else {
			//println("instForm:"+tx)
			val parts=tx.split('|')
			//println(parts(1))
			//println(parts(1).split(',').mkString(" "))
			if(parts.size!=2) NOFORMAT
			else 
				new InstFormat(parts(0),parts(1).split(',').map(_.toInt).toArray)			
		}
	}	
}
