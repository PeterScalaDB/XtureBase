/**
 * Author: Peter Started:22.05.2011
 */
package definition.typ

import scala.swing.Component
import definition.data.Reference

/** an editor component to be shown in the table browser for a certain kind of class
 * 
 */
trait CustomInstanceEditor {
	def getComponent:Component
	def load(ref:Reference):Unit
	def shutDown():Unit
	def editorName:String
}