/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ
import java.io.{DataInput,DataOutput}
/**
 * 
 */
class ParamAnswerDefinition(val name:String,val dataType:DataType.Value,val followQuestion:Option[ParamQuestion],
	val constraint:String="") {
	
	/*def write(out:DataOutput)= {
		out.writeUTF(name)
		out.writeInt(dataType.id)
		out.writeUTF(constraint)
		followQuestion match {
			case Some(question) =>out.writeBoolean(true); question.write(out)
			case None => out.writeBoolean(false)
		}		
		
	}*/
	def toXML():scala.xml.Node = {
		<Answer typ= {dataType.id.toString} name={name} con={constraint}>
		{followQuestion match {
			case Some(d)=> d.toXML()
			case None =>
		}}
		</Answer>
	}
	
	override def toString = "AnswerDef "+name+" "+dataType+" followQuestion:"+followQuestion

}

object ParamAnswerDefinition {
	def fromXML(node: scala.xml.Node):ParamAnswerDefinition = {
		val dtype=(node \"@typ").text.toInt
		val const=(node \"@con").text
		val question= ParamQuestion.fromXML(node) 
		new ParamAnswerDefinition((node \"@name").text,DataType(dtype),question,const)
	}
	
	/*def apply(in:DataInput):ParamAnswerDefinition = {
		new ParamAnswerDefinition(in.readUTF,DataType(in.readInt),if(in.readBoolean)Some(ParamQuestion(in)) else None,in.readUTF)
	}*/
}


	
	
