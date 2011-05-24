/**
 * Author: Peter Started:13.05.2011
 */
package definition.data

/**
 * 
 */
trait IntegrityChecker {
	def subInstanceChanged(ref:Reference)
	
	def shutDown()
	
	def load(baseRef:InstanceData)
}