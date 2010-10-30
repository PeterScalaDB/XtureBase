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
	
	private var hasResolved=false	
	val fields=new ArrayBuffer[FieldDefinition]() // list of inherited fields from the super class
	val propFields = new ArrayBuffer[PropertyFieldDefinition]()
	val superClassIDs:LinkedHashSet[Int] = LinkedHashSet()
	val actions=LinkedHashMap[String,AbstractAction]()
	val createActions=LinkedHashMap[String,AbstractAction]()
	val fieldEditors=LinkedHashSet[String]()
	
		
	def inheritsFrom(otherClassID:Int):Boolean =
  {
  	//println( " " +name +" "+id+"InheritsFrom: "+ otherClassID)
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
				superClassIDs ++=superClass.superClassIDs
				superClassIDs += id
				superClass.resolveSuperFields()
				fields ++= superClass.fields				
				propFields ++= superClass.propFields 
				actions ++=superClass.actions
				createActions ++=superClass.createActions
				fieldEditors ++=superClass.fieldEditors
			}	
			// add own fields
			for(a <- ownFields) {
					var ix= fields.findIndexOf(a.name==_.name)
					if(ix<0){						// not found
						
						fields += a
					}
					else fields(ix)=a
					if(a.editor.length>0)// editor defined
						fieldEditors +=a.editor
				}
			propFields ++=ownPropFields
			ownActions.foreach(a => actions(a.name)=a)	
			ownCreateActions.foreach(a => createActions(a.name)=a)
		  hasResolved=true
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}		
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString("\n")+"PropFields:\n"+propFields.mkString("\n");
	
	
	

}