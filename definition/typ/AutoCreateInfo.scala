/**
 * Author: Peter Started:14.05.2011
 */
package definition.typ

import definition.expression._

/** Information about children that should be automatically created when the instance is created
 * @param startValues Sequence of pairs (fieldNr,StartValue)
 */
case class AutoCreateInfo (propField:Byte,childType:Int,startValues:Seq[(Byte,Expression)]){
	
	def startValueToXML(value:(Byte,Expression)) = <sv field={value._1.toString} value={value._2.getTerm}/>
	
	def toXML = {
		<AutoCreate prop= {propField.toString} childType={childType.toString}> 
		{
			startValues map startValueToXML
		}
	  </AutoCreate>
	}
}

object AutoCreateInfo {
	def fromXML(node: scala.xml.Node) = 	{
		AutoCreateInfo((node \ "@prop").text.toByte,(node \ "@childType").text.toInt,
		(node \\ "sv") map startValueFromXML )
		 
	}
	
	def startValueFromXML(node: scala.xml.Node) = {
		val exText=(node \ "@value").text.trim
		((node \ "@field").text.toByte,if(exText.length==0) EMPTY_EX else StringParser.parse(exText))
	}
		
}