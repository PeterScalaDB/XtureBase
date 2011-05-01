/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import definition.data._

/** interface for all Controllers or Models that are to be controlled by a path controller
 * 
 */
trait PathControllable {
	/** opens the data 
	 * 
	 * @param parentRef the parent Ref of the data to open
	 * @param selectRef which child instance should be selected
	 * @param indent how much should the parent ref be indented in the path view
	 */
	def openData(parentRef:Reference,selectRef:Option[Reference],indent:Int)
	
	/** register a callback routine that notifies the PathController that a child
	 * reference should be opened 
	 * 
	 * @param callBack the call back routine of the PathController
	 */
	def registerOpenChildCallBack(callBack: (Reference)=> Unit)
	
	
}