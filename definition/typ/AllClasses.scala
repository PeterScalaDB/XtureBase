/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

import definition.data._
import collection.mutable.LinkedHashSet

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
  
  def getClassIDByName(aname:String):Int = getClassByName(aname).get.id
  
  // find a class by class ID
  def getClassByID(aId:Int )=if(classList.contains(aId)) classList(aId)
  else throw new IllegalArgumentException("getClassByID: ID "+aId+" not found")
  
  
  def fromXML(node: scala.xml.Node):Map[Int,B]
  
  // resolves all superfields from all classes. Will be called after reading all classes from XLM
  def resolveFields()=
  	 for(cl <-classList.valuesIterator){
  		 cl.resolveSuperFields()
  		 //System.out.println(cl.name+" "+cl.superClassIDs.mkString(","))
  }
  	
  /** gets the most common class that all classes inherit from
   * 
   * @param dataList list of referencable Instances
   * @return the id of the common class or -1 if there is no common class
   */
  def getCommonClass(dataList:Seq[Referencable]):Int = {
  	if(dataList==null || dataList.isEmpty) return -1
  	if(dataList.size==1) return dataList.first.ref.typ 
  	var aClassID= -2
  	var superClasses:LinkedHashSet[Int]=null
  	for(inst <-dataList) {
  		if(aClassID== -2){
  			aClassID=dataList.first.ref.typ
  			superClasses=getClassByID(aClassID).superClassIDs
  		}
  		else if(inst.ref.typ!=aClassID) {
  		  val otherSuperClasses=getClassByID(inst.ref.typ).superClassIDs
  		  superClasses=superClasses intersect otherSuperClasses
  		  if(superClasses.isEmpty) return -1
  		  aClassID=superClasses.last
  		}
  	}
  	aClassID
  }
  
  def getCommonClassForGroups(groupList:Seq[SelectGroup[_<:Referencable]]):Int = {
  	if(groupList==null || groupList.isEmpty) return -1
  	if(groupList.size==1&&groupList.first.children.size==1 ) return groupList.first.children .first.ref.typ 
  	var aClassID= -2
  	var superClasses:LinkedHashSet[Int]=null
  	for(group <-groupList)
  	for(inst <-group.children) {
  		if(aClassID== -2){
  			aClassID=inst.ref.typ
  			superClasses=getClassByID(aClassID).superClassIDs
  		}
  		else if(inst.ref.typ!=aClassID) {
  		  val otherSuperClasses=getClassByID(inst.ref.typ).superClassIDs
  		  superClasses=superClasses intersect otherSuperClasses
  		  if(superClasses.isEmpty) return -1
  		  aClassID=superClasses.last
  		}
  	}
  	aClassID
  }
}


class ClientClasses (node:scala.xml.Node) extends AllClasses [AbstractObjectClass] (node) {
	
	def fromXML(node: scala.xml.Node):Map[Int,AbstractObjectClass] =
  {  	
    (for (ac<- (node \\ "ObjectClass");oc= ClientObjectClass.fromXML(ac))	yield (oc.id -> oc)).toMap   
  }
}

case class SelectGroup[T <: Referencable](var parent:OwnerReference,var children:Seq[T]) 

object AllClasses  {	
	var classObj:AllClasses[_ <: AbstractObjectClass] = _
	def get =classObj
	def set(newObj:AllClasses[_ <: AbstractObjectClass]) = {
		classObj=newObj
		// Resove Superfields in all ClassVersions
    classObj.resolveFields()
	}
}