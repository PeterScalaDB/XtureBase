/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data.Reference

/**
 * Contains a list of all current classes
 */
abstract class AllClasses [B <:AbstractObjectClass] (node: scala.xml.Node)  {
	
		
  var classList:Map[Int,B]=  fromXML(node)
  
  
  // adds a new class to the list
  def addClass(cl:B)= 
  {
  	classList += (cl.id -> cl)
  	//for (ver <- cl.versions) ver.resolveSuperFields(this)
  }
  
  // get the Map object containing all classes
  def getClassList=classList
  
  // find a class by Name
  def getClassByName(aname:String):Option[AbstractObjectClass]= classList.valuesIterator.find(_.name ==aname) 
  
  // find a class by class ID
  def getClassByID(aId:Int )= classList(aId)     
  
  
  def fromXML(node: scala.xml.Node):Map[Int,B]
  
  // resolves all superfields from all classes. Will be called after reading all classes from XLM
  def resolveFields()= for(cl <-classList.valuesIterator)
  									 {
  											cl.resolveSuperFields()
  											cl.resolveSuperClassIDs()
  									 }		
}


class ClientClasses (node:scala.xml.Node) extends AllClasses [AbstractObjectClass] (node) {
	
	def fromXML(node: scala.xml.Node):Map[Int,AbstractObjectClass] =
  {  	
    (for (ac<- (node \\ "ObjectClass");oc= ClientObjectClass.fromXML(ac))	yield (oc.id -> oc)).toMap   
  }
}



object AllClasses  {	
	var classObj:AllClasses[_ <: AbstractObjectClass] = _
	def get =classObj
	def set(newObj:AllClasses[_ <: AbstractObjectClass]) = {
		classObj=newObj
		// Resove Superfields in all ClassVersions
    classObj.resolveFields()
	}
}