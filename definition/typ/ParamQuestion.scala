/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

/** Questions that Actions ask for to get their parameters
 * 
 */
case class ParamQuestion(name:String,possibleAnswers:Seq[ParamAnswerDefinition]) {
	def toXML ():scala.xml.Node = {
		println("question out " +name)
		<Question name={name} >
		{for(ans <-possibleAnswers) yield ans.toXML()}
		</Question>
	}
}

object ParamQuestion {
	def fromXML(node: scala.xml.Node):ParamQuestion = {
		val name=(node \"@name").text
		val answers=for(afield <-(node \"Answer")) yield ParamAnswerDefinition.fromXML(afield)
		new ParamQuestion(name,answers)
	}
	
	def makeQuestion(parms:List[(String,(String,DataType.Value))]):Option[ParamQuestion] = {
		val firstQ=parms.head
		new Some(ParamQuestion(firstQ._1,Seq(
			new ParamAnswerDefinition(firstQ._2._1,firstQ._2._2,if(parms.tail.isEmpty) None else makeQuestion(parms.tail)))))
	}
}