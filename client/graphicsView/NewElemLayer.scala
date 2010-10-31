/**
 * Author: Peter Started:31.10.2010
 */
package client.graphicsView
import definition.data.Reference
/**
 * 
 */
class NewElemLayer(ncontroller:GraphViewController) extends Layer	(ncontroller,new Reference(0,0),"NewElements",true,true){
  
  val myList=collection.mutable.ArrayBuffer[GraphElem]()
  elemList=myList
  
  def addTempElement(elem:GraphElem) ={
  	//println("elemList adding element "+elem)
  	elemList= (myList += elem)
  	controller.graphElemAdded(this,elem)  	
  }
  
  override def shutDown= {
  	//println("elemList clear")
  	//Thread.dumpStack
  	myList.clear
  	//controller.layerChanged(this)  	
  }
  
}