/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

/** superclass of all user-defined modules defining actions for classes
 * 
 */
trait ActionModule {
	def getActionCount:Int
	
	def getAction(ix:Int):ActionImpl
}

object ActionModule {
	def load(moduleName:String):ActionModule = {
		Class.forName(moduleName).newInstance.asInstanceOf[ActionModule]
	}
}