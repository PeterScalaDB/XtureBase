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
class ObjectClass (val name:String,val id:Int,val description:String,fields:Seq[FieldDefinition],
	val propFields:Seq[PropertyFieldDefinition], val superClasses:Seq[String])
{  
 
  private var vsuperFields:Seq[FieldDefinition]= IndexedSeq.empty // list of inherited fields from the super class
	private var vsuperPropFields:Seq[PropertyFieldDefinition] = IndexedSeq.empty
	private var vsuperClassIDs:Seq[Int] = IndexedSeq.empty
	
	//def superFields=vsuperFields
	//def superPropFields=vsuperPropFields
	def superClassIDs=vsuperClassIDs // Id ofs this class and all super classes
  
  
  /** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference]):InstanceData =
  {
  	new InstanceData(ref,Array.fill(getFieldCount)(EMPTY_EX),owner)
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
  
  /** checks if this class inherits from the given other class
   * 
   * @param otherClassID id of the other class
   * @return true if this is a subclass of otherClassID
   */
  def inheritsFrom(otherClassID:Int):Boolean =
  {
  	println( " " +name +" "+id+"InheritsFrom: "+ otherClassID)
  	superClassIDs.contains(otherClassID)
  }
  
  //TODO: check class Version when testing inheritance. InstanceProperties needs to store the class versions
  

	
	
	// converts the description of this version to XML
	def toXML =
	{
		val sc=superClasses.map { a =>  <sc name= { a }  /> }		
		<ObjectClass name={name} id={id.toString} desc={description}>
		<Fields> 		{			fields.map(i => i.toXML)	}</Fields>
		<PropFields> 		{			propFields.map(i => i.toXML)	}</PropFields>
		<SuperClasses> {  sc }</SuperClasses>
		</ObjectClass>
	}	
	
	// gets the inherited fields from the super classes
	def resolveSuperFields():Unit = 	{		
		if(vsuperFields.isEmpty)
		{	
			for(cl <-superClasses)
				AllClasses.getClassByName(cl).get.resolveSuperFields()
		  vsuperFields= superClasses.flatMap( AllClasses.getClassByName(_).get.getFields  )
		  vsuperPropFields = superClasses.flatMap( AllClasses.getClassByName(_).get.getPropFields  )		  
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}	
	
	// creates a list of all super classes of this class
	def resolveSuperClassIDs():Seq[Int] = {
		if(vsuperClassIDs.isEmpty)
		{			
			vsuperClassIDs=superClasses.flatMap(AllClasses.getClassByName(_).get.resolveSuperClassIDs) :+ id
			vsuperClassIDs
		} 
		//println("ClassVersion" +classID+" vers:"+versNr+" superClasses:"+vsuperClassIDs)
		else vsuperClassIDs
	}
		
	
	private def getFields :Seq[FieldDefinition] = vsuperFields ++ fields
	private def getPropFields : Seq[PropertyFieldDefinition] = vsuperPropFields ++ propFields
	
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
	
	def getPropFieldCount = vsuperPropFields.length+propFields.length
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString("\n")+"PropFields:\n"+propFields.mkString("\n");
}

object ObjectClass 
{	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node) =
	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		new ObjectClass(name,id ,  (node \"@desc").text,
			for(afield <-(node \\"FieldDef")) yield FieldDefinition.fromXML(afield),
		  for(bfield <-(node \\"PropertyFieldDef")) yield PropertyFieldDefinition.fromXML(bfield),
		  for(cfield <-(node \\ "sc"))  yield (cfield \ "@name").text 			 )
	}
}




