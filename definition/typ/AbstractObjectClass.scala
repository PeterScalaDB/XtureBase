/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import definition.data._
import collection.mutable.{HashSet,ArrayBuffer,LinkedHashMap}


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
	protected def superClasses:Seq[String]
	
	private var hasResolved=false	
	val fields=new ArrayBuffer[FieldDefinition]() // list of inherited fields from the super class
	val propFields = new ArrayBuffer[PropertyFieldDefinition]()
	val superClassIDs:HashSet[Int] = HashSet(id)
	val actions=LinkedHashMap[String,AbstractAction]()
	
	
		
	def inheritsFrom(otherClassID:Int):Boolean =
  {
  	println( " " +name +" "+id+"InheritsFrom: "+ otherClassID)
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
				superClass.resolveSuperFields()
				fields ++= superClass.fields
				
				propFields ++= superClass.propFields 
				actions ++=superClass.actions					
			}	
			// add own fields
			for(a <- ownFields) {
					val ix= fields.findIndexOf(a.name==_.name)
					if(ix<0) // not found
						fields += a
						else fields(ix)=a
				}
			propFields ++=ownPropFields
			ownActions.foreach(a => actions(a.name)=a)		  
		  hasResolved=true
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}		
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString("\n")+"PropFields:\n"+propFields.mkString("\n");
	
	
	

}