/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import definition.data._

/** interface for all Controllers or Models that are to be controlled by a path controller
 * 
 */
trait PathControllable {
	def openData(parentRef:Reference,selectRef:Option[Reference])
	def registerOpenChildCallBack(callBack: (Reference)=> Unit)
}