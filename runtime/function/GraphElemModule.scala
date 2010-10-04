/**
 * Author: Peter Started:04.10.2010
 */
package runtime.function

import server.storage.{ActionModule,ActionImpl,ActionIterator}
import scala.collection.Iterator
import definition.expression.Constant
import definition.data.InstanceData

/**
 * 
 */
class GraphElemModule extends ActionModule {
  
	def actionList=List(moveAction)
	
  def getActionsIterator() = { actionList.iterator }
  
  val moveAction=new ActionImpl("Verschieben",None,nope)
  
  def nope( data:InstanceData,param:Seq[(String,Constant)]) =  {true}
}

class LineModule extends GraphElemModule {
	
}