/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

/**
 * 
 */
class ParamAnswerDefinition(val name:String,val dataType:DataType.Value,val followQuestion:Option[ParamQuestion]) {
	
	def toXML():scala.xml.Node = {
		<Answer typ= {dataType.id.toString} name={name}>
		{followQuestion match {
			case Some(d)=> d.toXML()
			case None =>
		}}
		</Answer>
	}

}

object ParamAnswerDefinition {
	def fromXML(node: scala.xml.Node):ParamAnswerDefinition = {
		val dtype=(node \"@typ").text.toInt
		val question=for(qfield <-(node \"Question")) yield ParamQuestion.fromXML(qfield) 
		new ParamAnswerDefinition((node \"@name").text,DataType(dtype),if(question.isEmpty)None else Some(question.first))
	}
	
	
}