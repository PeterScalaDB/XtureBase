/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

/** describes an Action of a Class
 * 
 */

trait AbstractAction {
	def name:String
	def question:Option[ParamQuestion]=None
	def isIterator:Boolean
	
	def toXML:scala.xml.Elem
	
	override def toString = "Action ["+(if(name==null) "null" else name)+"] question:"+question
}


class ActionDescription(val name:String,override val question:Option[ParamQuestion],val isIterator:Boolean) extends AbstractAction {
  def toXML:scala.xml.Elem=null
}

object ActionDescription {
	def fromXML(node: scala.xml.Node) = {
		val name=(node \"@name").text
		val isIt=(node \"@iter").text=="1"
		val questions=for(afield <-(node \"Question")) yield ParamQuestion.fromXML(afield)
		new ActionDescription(name,if(questions.isEmpty) None else Some(questions.first),isIt)
	}
}