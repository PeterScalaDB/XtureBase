/**
 * Author: Peter Started:30.10.2010
 */
package client.dialog

import definition.data.Referencable

/** abstract superclass for newPanelArea
 *  Container Listeners are notified when a Container of elements is focused
 *  the NewPanel Area can show appropriate CreateActions to create new elements for that container
 */
trait ContainerFocusListener {
	def containerFocused(superInst:Referencable, propField:Int,containerName:String="")
}