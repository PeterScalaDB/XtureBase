/**
 * Author: Peter Started:21.09.2010
 */
package server.storage

import definition.typ._

/** Server-side class list to be set as AllClasses instance
 * 
 */
class ServerClassList (node: scala.xml.Node) extends AllClasses [ServerObjectClass](node) {			
	//override val classList:Map[Int,ServerObjectClass]=_
	
	override def fromXML(node: scala.xml.Node):Map[Int,ServerObjectClass]=
  {
  	if(node==null) Map[Int,ServerObjectClass]()
  	else (for (ac<- (node \\ "ObjectClass");oc= ServerObjectClass.fromXML(ac))	yield (oc.id -> oc)).toMap          	
  }
	
	def toXML()=
  {
  	<ClassList> {for (c<-classList.valuesIterator) yield c.toXML()  }  </ClassList>
  }
	
	def saveToXML()=
  {
  	<ClassList> {for (c<-classList.valuesIterator) yield c.saveToXML()  }  </ClassList>
  }
	
	
	
}