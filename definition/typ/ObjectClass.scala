/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data._
import definition.expression.EMPTY_EX

/**
 *  description of a Class
 */
class ObjectClass (val name:String,val id:Int,val description:String,nversions:List[ClassVersion])
{
  private var pversions=nversions //list of versions
  private var plastVersion=getLastVersion(nversions) 
  
  def versions=pversions
  def lastVersion=plastVersion
  
  // returns the last version of this class
  private def getLastVersion(lversions:List[ClassVersion])= 
  {
  	lversions.reduceLeft((a,b)=> if(a.versNr >b.versNr )a else b)
  }
  
  // adds a version to this class
  def addVersion(n:ClassVersion) =
  {
  	pversions=n :: pversions
  	plastVersion=getLastVersion(pversions)
  }
  
  // gets the version with number ix
  def getVersion(ix:Byte)=
  {
  	if(ix==0) Some(lastVersion)
  	else pversions.find(i=>i.versNr==ix)
  }
  
  // converts the description of this class to xml
  def toXML =
  {
  	<ObjectClass name={name} id={id.toString} desc={description}>
  	{ pversions.map(i => i.toXML) }
  	</ObjectClass>
  }  
  
  /** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference]):InstanceData =
  {
  	new InstanceData(ref,lastVersion.versNr,Array.fill(lastVersion.getFieldCount)(EMPTY_EX),owner)
  }
  
  /** creates an empty InstanceProperty of this class
   * 
   * @param ref the instance of that Property list
   * @return the new Property object
   */
  def createInstanceProperty(ref:Reference):InstanceProperties =
  {
  	val pArray=(for(i <- 0 until lastVersion.getPropFieldCount) 
  		yield new PropertyFieldData(lastVersion.propField(i).single,Nil)).toArray
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
  	plastVersion.superClassIDs.contains(otherClassID)
  }
  
  //TODO: check class Version when testing inheritance. InstanceProperties needs to store the class versions
  
}

object ObjectClass 
{	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node) =
	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		new ObjectClass(name,id ,  (node \"@desc").text,
			(for(afield <-(node \\"ClassVersion"))
				yield ClassVersion.fromXML(afield,id)).toList )
	}
}


// ------------------------------------------------------------------------------------------------------------

/**
 *  a Version of a class description
 */
class ClassVersion (val versNr:Byte, 
	private val fields:Array[FieldDefinition],private val propFields:Array[PropertyFieldDefinition], 
	val superClasses:List[(String,Byte)],classID:Int)
{	
	private var vsuperFields:Array[FieldDefinition]= null // list of inherited fields from the super class
	private var vsuperPropFields:Array[PropertyFieldDefinition] = null
	private var vsuperClassIDs:List[Int] = Nil
	
	def superFields=vsuperFields
	def superPropFields=vsuperPropFields
	def superClassIDs=vsuperClassIDs // Id ofs this class and all super classes
	
	// converts the description of this version to XML
	def toXML =
	{
		val sc=superClasses.map { case(name,ver) => { <sc name= {name } version={ver.toString} /> }}		
		<ClassVersion versNr={versNr.toString}>
		<Fields> 		{			fields.map(i => i.toXML)	}</Fields>
		<PropFields> 		{			propFields.map(i => i.toXML)	}</PropFields>
		<SuperClasses> {  sc }</SuperClasses>
		</ClassVersion>
	}	
	
	// gets the inherited fields from the super classes
	def resolveSuperFields(ac:AllClasses.type):Unit = 	{		
		if(superFields==null)
		{	
			for(cl <-superClasses)
				ac.getClassVersion(cl._1,cl._2).resolveSuperFields(ac)
		  vsuperFields= superClasses.flatMap({case(sname,sver) => { ac.getClassVersion(sname,sver).getFields  }}).toArray
		  vsuperPropFields = superClasses.flatMap({case(sname,sver) => { ac.getClassVersion(sname,sver).getPropFields  }}).toArray		  
		}
		//Console.println("Resolve "+versNr+" "+superClasses+" "+vsuperFields)  
	}	
	
	// creates a list of all super classes of this class
	def resolveSuperClassIDs(ac:AllClasses.type):List[Int] = {
		if(vsuperClassIDs==Nil)
		{			
		  for(cl <-superClasses)							 
				vsuperClassIDs= ac.getClassVersion(cl._1,cl._2).resolveSuperClassIDs(ac) ::: vsuperClassIDs
		  vsuperClassIDs=classID :: vsuperClassIDs	
		} 
		//println("ClassVersion" +classID+" vers:"+versNr+" superClasses:"+vsuperClassIDs)
		vsuperClassIDs
	}
		
	
	def getFields :Array[FieldDefinition] = vsuperFields++ fields
	def getPropFields : Array[PropertyFieldDefinition] = vsuperPropFields ++ propFields
	
	def field(ix:Int) =
	{
		
		if (ix<superFields.length)superFields(ix)
		else fields(ix-superFields.length)
	}
	
	def getFieldCount = superFields.length+fields.length
	
	def propField(ix:Int) =
	{
		
		if (ix<superPropFields.length)superPropFields(ix)
		else propFields(ix-superPropFields.length)
	}
	
	def getPropFieldCount = superPropFields.length+propFields.length
	
	
	override def toString = "ClassVersion "+versNr+" Fields:\n"+fields.mkString("\n");
}



object ClassVersion
{	
	
	// creates a version from XLM
	def fromXML(node: scala.xml.Node,thisID:Int)=
	{
		val a:Array[FieldDefinition]= (for(afield <-(node \\"FieldDef"))
				      yield FieldDefinition.fromXML(afield) ).toArray
	  val b:Array[PropertyFieldDefinition] = (for(bfield <-(node \\"PropertyFieldDef"))
				      yield PropertyFieldDefinition.fromXML(bfield) ).toArray 
		
		 new ClassVersion( (node \ "@versNr").text.toByte, a,b,
			(for(cfield <-(node \\ "sc")) 
				      yield ((cfield \ "@name").text, (cfield \ "@version").text.toByte) ).toList ,thisID)
	  
	}
}

