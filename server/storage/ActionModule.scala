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
}

object ActionModule {
	def load(moduleName:String):ActionModule = {
		Class.forName(moduleName).newInstance.asInstanceOf[ActionModule]
	}
}