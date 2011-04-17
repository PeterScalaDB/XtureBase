/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import definition.data._
import collection.mutable.{HashMap,ArrayBuffer,LinkedHashMap,LinkedHashSet}
import definition.typ.form.FormBox


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
	protected def superClasses:Seq[Int]	
	protected def ownFieldSettings:Seq[FieldSetting]
	
	private var hasResolved=false	
	val fields=new ArrayBuffer[FieldDefinition]() // list of inherited fields from the super class
	val propFields = new ArrayBuffer[PropertyFieldDefinition]()
	val superClassIDs:LinkedHashSet[Int] = LinkedHashSet()
	val actions=LinkedHashMap[String,AbstractAction]()
	val createActions=LinkedHashMap[String,AbstractAction]()
	val fieldEditors=LinkedHashSet[String]()
	protected val fieldSettingSet=LinkedHashMap[Int,FieldSetting]()
	def shortFormat:InstFormat
	def longFormat:InstFormat
	def resultFormat:InstFormat
	
	def formBox:Option[FormBox]
	
	lazy val propFieldIndices=propFields.indices.map(x=>(propFields(x).name -> x )).toMap
		
	def inheritsFrom(otherClassID:Int):Boolean =
  {
  	//System.out.println( name +" "+id+"InheritsFrom: "+ otherClassID)
  	//println(superClassIDs.mkString(","))
  	superClassIDs.contains(otherClassID)
  }
	
	def resolveSuperFields():Unit = 	{		
		if(!hasResolved)
		{	
			for(cl <-superClasses)
			{
				val superClass:AbstractObjectClass= AllClasses.get.getClassByID(cl) 
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
		//Console.System.out.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}	
	
	
	
	def copySuperClassFields(superClass:AbstractObjectClass) = {
		var fieldNum = fields.size
		fields++=superClass.fields
		for(i <- superClass.fields.indices)
		  if(superClass.fieldSettingSet.contains(i))
		  	fieldSettingSet(fieldNum+i)= superClass.fieldSettingSet(i)
	}
	
	def fieldSetting(fieldNr:Int):FieldSetting= {
	if(fieldSettingSet.contains(fieldNr) ) fieldSettingSet(fieldNr)
	else EmptySetting	
	}
	
	def getFieldSettingsList:Seq[FieldSetting]= for(i<-0 until fields.size) yield fieldSetting(i)
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString(",")+"\nPropFields:\n"+propFields.mkString("\n")+
	"\nSuperclasses:"+ superClassIDs.mkString(",");	
	
	
	def getNum_FirstHiddenPropFields:Int = {
		for(i <-propFields.indices)
			if(!propFields(i).hidden )return i
		return propFields.size
	}
	
	def getPropFieldByName(name:String):Option[PropertyFieldDefinition]= {
		//if (propFieldIndices.contains(name)) Some(propFields(propFieldIndices(name)))
		//else None
		propFieldIndices.get(name) map (propFields(_))
	}
	
}



case class InstFormat(val formStr:String,val fields:Array[Int]){
	override def toString= formStr+"|"+fields.mkString(",")
}
object NOFORMAT extends InstFormat("",Array.empty) {
	override def toString=""
}

object InstFormat {
	def fromString(tx:String)= {
		if(tx=="") NOFORMAT
		else {			
			val parts=tx.split('|')
			
			if(parts.size!=2) NOFORMAT
			else 
				new InstFormat(parts(0),parts(1).split(',').map(_.toInt).toArray)			
		}
	}	
	def read(node: scala.xml.NodeSeq) = fromString(node.text)
		
}
