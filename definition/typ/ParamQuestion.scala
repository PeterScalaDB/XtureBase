/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import java.io.{DataInput,DataOutput,ByteArrayInputStream,DataInputStream}

/** Questions that Actions ask for to get their parameters
 * 
 */


abstract class ParamQuestion {
	//def write(out:DataOutput):Unit
	def toXML ():scala.xml.Node
}


case class DialogQuestion(name:String,possibleAnswers:Seq[ParamAnswerDefinition],repeat:Boolean=false) extends ParamQuestion {	
	/*def write(out:DataOutput) = {
		out.writeByte(1)
		out.writeUTF(name)		
		out.writeShort(possibleAnswers.size)
		for(p <-possibleAnswers)
			p.write(out)
		out.writeBoolean(repeat)
	}*/
	def toXML ():scala.xml.Node = {
		<DialogQuestion name={name} repeat={if(repeat)"1" else "0"} >
		{for(ans <-possibleAnswers) yield ans.toXML()}
		</DialogQuestion>
	}
	
}

object ParamQuestion {	
	/*def apply(in:DataInput):ParamQuestion = {
		in.readByte match {
			case 1 => DialogQuestion(in.readUTF,for(i <-0 until in.readShort) yield ParamAnswerDefinition(in),in.readBoolean)
			case 2 => {
				val module=in.readUTF
				val size=in.readInt
				val buffer=new Array[Byte](size)
			  in.readFully(buffer,0,size)
				CustomQuestion(module,buffer)
			}
			case a => throw new IllegalArgumentException("Unknown Question-Type "+a)
		}*/
		
	def fromXML(superNode: scala.xml.Node):Option[ParamQuestion] = {
		var dnode=(superNode\\"DialogQuestion")
		if(dnode.isEmpty){ 
			dnode=(superNode\\"CustomQuestion")
			if(dnode.isEmpty) None
			else {
				 val subNode=dnode.first
				 Some(new CustomQuestion((subNode \"@moduleName").text,subNode.child))
			}
		}
		else {
			val node=dnode.first
			val name=(node \"@name").text
			val repeat=(node \"@repeat").text=="1"
			val answers=for(afield <-(node \"Answer")) yield ParamAnswerDefinition.fromXML(afield)
			Some(new DialogQuestion(name,answers,repeat))
		}				
	}	
	
	def makeQuestion(parms:List[(String,(String,DataType.Value))]):Option[ParamQuestion] = {
		val firstQ=parms.head
		new Some(DialogQuestion(firstQ._1,Seq(
			new ParamAnswerDefinition(firstQ._2._1,firstQ._2._2,if(parms.tail.isEmpty) None else makeQuestion(parms.tail)))))
	}
}

case class CustomQuestion(moduleName:String, customData:Seq[scala.xml.Node]) extends ParamQuestion {
	/*def write(out:DataOutput)= {
		out.writeByte(2)
		out.writeInt(customData.size)
		out.write(customData)
		def toXML ():scala.xml.Node
	}*/
	
	//def getInputStream= new DataInputStream(new ByteArrayInputStream(customData))
	def toXML ():scala.xml.Node = {
		<CustomQuestion moduleName={moduleName}  >
		{customData}
		</CustomQuestion>
	}
	
}