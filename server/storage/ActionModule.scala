/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.typ.AbstractAction

/** superclass of all user-defined modules defining actions for classes
 * 
 */
trait ActionModule {	
	def getActionsIterator:Iterator[AbstractAction]
	def getCreateActionsIterator:Iterator[AbstractAction]=Seq.empty.iterator
}

object ActionModule {
	def load(moduleName:String):ActionModule = {
		Class.forName(moduleName).newInstance.asInstanceOf[ActionModule]
	}
}