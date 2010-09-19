/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data.Reference

/**
 * Contains a list of all current classes
 */
object AllClasses {
  private var classList=Map[Int,ObjectClass]()
  
  // adds a new class to the list
  def addClass(cl:ObjectClass)= 
  {
  	classList=classList + (cl.id -> cl)
  	//for (ver <- cl.versions) ver.resolveSuperFields(this)
  }
  
  // get the Map object containing all classes
  def getClassList=classList
  
  // find a class by Name
  def getClassByName(aname:String):Option[ObjectClass]= classList.valuesIterator.find(_.name ==aname) 
  
  // find a class by class ID
  def getClassByID(aId:Int )= classList(aId)
  
    
  	
      
  def toXML()=
  {
  	<ClassList> {for (c<-classList.valuesIterator) yield c.toXML  }  </ClassList>
  }
  
  def fromXML(node: scala.xml.Node)=
  {
  	if(classList.isEmpty)
    classList= (for (ac<- (node \\ "ObjectClass");oc= ObjectClass.fromXML(ac))	yield (oc.id -> oc)).toMap    	           
    
    // Resove Superfields in all ClassVersions
    resolveFields      	
  }
  
  // resolves all superfields from all classes. Will be called after reading all classes from XLM
  def resolveFields= for(cl <-classList.valuesIterator)
  									 {
  											cl.resolveSuperFields()
  											cl.resolveSuperClassIDs()
  									 }		
}