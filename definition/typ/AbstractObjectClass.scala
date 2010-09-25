/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import definition.data._

/**
 * 
 */
trait AbstractObjectClass {
	
	def name:String
	
	def description:String
	
	def id:Int
	
	protected def fields:Seq[FieldDefinition]
	protected def propFields:Seq[PropertyFieldDefinition] 
	protected def actions:Seq[AbstractAction]
	protected def superClasses:Seq[String]
	
		
	protected var vsuperFields:Seq[FieldDefinition]= IndexedSeq.empty // list of inherited fields from the super class
	protected var vsuperPropFields:Seq[PropertyFieldDefinition] = IndexedSeq.empty
	protected var vsuperClassIDs:Seq[Int] = IndexedSeq.empty
	protected var superClassActions:Seq[AbstractAction] = IndexedSeq.empty
	
	
	def superClassIDs:Seq[Int]=vsuperClassIDs
	
	def inheritsFrom(otherClassID:Int):Boolean =
  {
  	println( " " +name +" "+id+"InheritsFrom: "+ otherClassID)
  	superClassIDs.contains(otherClassID)
  }
	
	def resolveSuperFields():Unit = 	{		
		if(vsuperFields.isEmpty)
		{	
			for(cl <-superClasses)
				AllClasses.get.getClassByName(cl).get.resolveSuperFields()
		  vsuperFields= superClasses.flatMap( AllClasses.get.getClassByName(_).get.getFields  )
		  vsuperPropFields = superClasses.flatMap( AllClasses.get.getClassByName(_).get.getPropFields  )
		  superClassActions= superClasses.flatMap (AllClasses.get.getClassByName(_).get.getActions)
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}	
	
	// creates a list of all super classes of this class
	def resolveSuperClassIDs():Seq[Int] = {
		if(vsuperClassIDs.isEmpty)
		{			
			vsuperClassIDs=superClasses.flatMap(AllClasses.get.getClassByName(_).get.resolveSuperClassIDs) :+ id
			vsuperClassIDs
		} 
		//println("ClassVersion" +classID+" vers:"+versNr+" superClasses:"+vsuperClassIDs)
		else vsuperClassIDs
	}
	
	protected def getFields :Seq[FieldDefinition] = vsuperFields ++ fields
	protected def getPropFields : Seq[PropertyFieldDefinition] = vsuperPropFields ++ propFields
	def getActions = superClassActions++actions
	
	def field(ix:Int):FieldDefinition =
	{		
		if (ix<vsuperFields.length)vsuperFields(ix)
		else fields(ix-vsuperFields.length)
	}
	
	def getFieldCount = vsuperFields.length+fields.length
	
	def propField(ix:Int) =
	{
		
		if (ix<vsuperPropFields.length)vsuperPropFields(ix)
		else propFields(ix-vsuperPropFields.length)
	}
	
	def action(ix:Int) = if (ix<superClassActions.length)superClassActions(ix)
			else actions(ix-superClassActions.length)
	
	def getPropFieldCount = vsuperPropFields.length+propFields.length
	
	def getActionCount = superClassActions.length+actions.length	
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString("\n")+"PropFields:\n"+propFields.mkString("\n");
	
	
	

}