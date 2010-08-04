/**
 * Author: Peter Started:28.07.2010
 */
package definition.data

/** Super class for Property Managers
 * will be subclassed by a local and a remote version
 * 
 */
trait APropertyManager {
	
	def addProperty(instRef:Reference,newPropInstance:Reference) = {}
	
	
	def removeProperty(instRef:Reference,propInstance:Reference) = {}
	
	

}