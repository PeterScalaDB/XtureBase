/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ

import scala.swing.Alignment
/**
 * 
 */
object HorAlign extends Enumeration {
	val Left=Value("Left")
	val Right=Value("Right")
	val Center=Value("Center")
	val Block=Value("Block")
	
	def toScalaAlignment(a:Value)= {
		a match {
			case Left=> Alignment.Left 
			case Right=> Alignment.Right
			case Center=> Alignment.Center
			case _ => Alignment.Left
		}
	}
}