/**
 * Author: Peter Started:10.10.2010
 */
package definition.typ

import scala.swing.Panel
import definition.data._

/** abstract definition of a field editor
 * 
 */
trait FieldEditor {
	def getPanel:Panel
	def setData(data:Seq[SelectGroup[_<:Referencable]])
}