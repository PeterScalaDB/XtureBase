/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

/** Questions that Actions ask for to get their parameters
 * 
 */
case class ParamQuestion(name:String,possibleAnswers:Seq[ParamAnswerDefinition],repeat:Boolean=false) {
	def toXML ():scala.xml.Node = {
		//println("question out " +name)
		<Question name={name} repeat={if(repeat)"1" else "0"} >
		{for(ans <-possibleAnswers) yield ans.toXML()}
		</Question>
	}
}

object ParamQuestion {
	def fromXML(node: scala.xml.Node):ParamQuestion = {
		val name=(node \"@name").text
		val repeat=(node \"@repeat").text=="1"
		val answers=for(afield <-(node \"Answer")) yield ParamAnswerDefinition.fromXML(afield)
		new ParamQuestion(name,answers,repeat)
	}
	
	def makeQuestion(parms:List[(String,(String,DataType.Value))]):Option[ParamQuestion] = {
		val firstQ=parms.head
		new Some(ParamQuestion(firstQ._1,Seq(
			new ParamAnswerDefinition(firstQ._2._1,firstQ._2._2,if(parms.tail.isEmpty) None else makeQuestion(parms.tail)))))
	}
}