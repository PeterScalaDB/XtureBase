/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ
import java.io.{DataInput,DataOutput}

/** describes an Action of a Class
 * 
 */

trait AbstractAction {
	def name:String
	def question:Option[ParamQuestion]=None
	def isIterator:Boolean
	
	
	//def write(out:DataOutput)
	def toXML:scala.xml.Elem
	
	override def toString = "Action ["+(if(name==null) "null" else name)+"] question:"+question
}


class ActionDescription(val name:String,override val question:Option[ParamQuestion],val isIterator:Boolean) extends AbstractAction {
  def toXML:scala.xml.Elem=null
	//def write(out:DataOutput)= {}
}

object ActionDescription {
	/*def apply(in:DataInput) = {
		new ActionDescription(in.readUTF,if(in.readBoolean)Some(ParamQuestion(in)) else None,in.readBoolean)
	}*/
	
	def fromXML(node: scala.xml.Node) = {
		val name=(node \"@name").text
		val isIt=(node \"@iter").text=="1"		
		new ActionDescription(name,ParamQuestion.fromXML(node),isIt)
	}
}