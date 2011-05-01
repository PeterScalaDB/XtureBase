/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ.form
import definition.data.{Reference, InstanceData}
import definition.expression.{StringParser,Constant,EMPTY_EX}
import definition.typ.HorAlign
import definition.expression.Expression
import definition.expression.StringConstant
import scala.swing.Component
import java.awt.Dimension
import scala.collection.JavaConversions._
import scala.xml.UnprefixedAttribute
import scala.xml.Elem
import scala.xml.{MetaData,Text,Attribute}
import scala.xml.Null
import java.awt.Color

/** Super class of all screen form elements
 * 
 */
trait FormElement {
   def minWidth:Int
   def maxWidth:Int
   def minHeight:Int
   def maxHeight:Int 
   def toXML:scala.xml.Node   
   
   type InitFunc=(FormElement)=>Unit
   
   def setupComponent(comp:Component)={
  	 if (minWidth>0&&minHeight>0) {
  		 comp.minimumSize= new Dimension(minWidth,minHeight)
  		 comp.preferredSize=comp.minimumSize
  	 }
  	 if(maxWidth!=0||maxHeight!=0) 
  		 comp.maximumSize= new Dimension(if(maxWidth<1)Short.MaxValue else maxWidth,
  			 if(maxHeight<1)Short.MaxValue else maxHeight)  	 
   }
   
   def getProps:List[(String,String)]= {
  	 val node=this.toXML
  	 node.attributes.map(a=> (a.key,a.value.text)).toList.sortWith((a,b)=> a._1 < b._1)
  	 //node.attributes
   }   
   
   def updateProperty(propName:String,newValue:String,inf:Option[InitFunc]=None) = {
  	 val node=this.toXML.asInstanceOf[Elem]
     val newAttribs= changeAttribute(node.attributes,propName,newValue)
  	 //println("NA:" + newAttribs.mkString("\n"))
  	 val newXML=node.copy(attributes=newAttribs)
  	 //println(newXML)
  	 val ne=FormElement.readElement(newXML)
  	 for (i<-inf) {
  		 ne match {
  			 case fb:FormBox=> fb.foreach(i)
  			 case other => i(other)
  		 }
  	 }
  	 ne
  		 
   }
   
   private def changeAttribute(attr:MetaData,key:String,newValue:String):MetaData= {
  	 //println("CA:"+attr.getClass)
  	 if(attr==Null) return attr
  	 if(attr.key.equalsIgnoreCase(key)) Attribute(attr.key,Text(newValue),changeAttribute(attr.next,key,newValue))
  	 else Attribute(attr.key,attr.value,changeAttribute(attr.next,key,newValue))
   }
   
   private implicit def iterableToMetaData(items: Iterable[MetaData]): MetaData = {
  	 items match {
  		 case Nil => null
  		 case head :: tail => head.copy(next=iterableToMetaData(tail))
  	}    
   }
   
   def select(selCol:Color):Unit= {
  	 if(! FormElement.backgroundMap.contains(this.getClass) ) 
  		 FormElement.backgroundMap(this.getClass)=this.asInstanceOf[Component].background
  	 this.asInstanceOf[Component].background=selCol
  }
   
  def deselect: Unit = {
  	//println("back:"+FormElement.backgroundMap.mkString(",")+" "+this.getClass+" "+FormElement.backgroundMap(this.getClass))
  	this.asInstanceOf[Component].background=
  		if( FormElement.backgroundMap.contains(this.getClass) ) 
    		 FormElement.backgroundMap(this.getClass)
    	else Color.yellow
  } 
  
  def makeCopy: FormElement
  
  
}

/** Interface for an mechanism to handle updates of fields 
 * 
 */
trait DataChangeListener { 
	def fieldChanged(field:Byte,newValue:Expression)
	def parseValue(fieldNr:Byte,text:String):Expression
}

trait FormDataField extends FormElement {
	var listener:Option[DataChangeListener]=None
	def fieldNr:Byte
	//def updateData(ndata:InstanceData):Unit
	def wantShutDown:Unit
	def shutDown:Unit
	def setDataValue(dvalue:InstanceData)
	def setListener (nlist:Option[DataChangeListener])= {
		listener=nlist
	}
	//override def getProperties=("field",fieldNr.toString):: super.getProperties
	
}


	
object FormElement {	
	val formColor=new Color(210,210,215)
	
	val backgroundMap=collection.mutable.Map[Class[_],Color]()
	//var listener:DataChangeListener=NullListener
	val validFormElements=List("FormLabel","TextField","FormBox","CalcField")
	
	object NullListener extends DataChangeListener {
     def fieldChanged(field:Byte,newValue:Expression) = 
    	 System.out.println("Reactor not initialized  field:" +field+" "+newValue)		
     def parseValue(fieldNr:Byte,text:String):Expression = {
    	 new StringConstant(text)
     }
	}
	object EmptyNode extends Elem (null,"",Null,xml.TopScope,null)
  
  
	def readElement(node: scala.xml.Node):FormElement = {
		node.label match {
			case "FormLabel" => getFormLabel(node)
			case "TextField" => getTextField(node)
			case "FormBox" => FormBox(node)
			case "CalcField" => getCalcField(node)
			case other => throw new IllegalArgumentException("Unknown Form element :"+other)
		}
	}	
	
	def readElements(node:xml.Node):Seq[FormElement] = {
		node.child.filter(n=>validFormElements.contains(n.label)). map(anode=> FormElement.readElement(anode))
		/*val labels=for(labelNode <-(node \"FormLabel")) yield FormElement.getFormLabel(labelNode)
		val textFields=for(textNode <-(node \"TextField")) yield FormElement.getTextField(textNode)		
		val boxes=for(boxNode <-(node \"FormBox")) yield FormBox(boxNode)
		labels++ textFields++ boxes*/
	}
	
	def getElementByName(name:String) = {
		name match {
			case "FormLabel" => new FormLabel(0,0,20,0,"Label",HorAlign.Left )
			case "TextField" => new FormTextField(80,0,30,30,HorAlign.Left,0)
			case "FormBox" => FormBox(EmptyNode)
			case "CalcField" => new FormCalcField(0,0,20,0,HorAlign.Left,StringConstant("Calc"))
			case other => throw new IllegalArgumentException("Unknown Form element :"+other)
		}
	}
	
	def parseInt(value:String):Int = try {
  	  value.toInt 
   } catch {case e => 0 }
   
  def getFormLabel(node:scala.xml.Node) =  new FormLabel(FormElement.parseInt((node \"@minWidth").text),FormElement.parseInt((node \"@maxWidth").text),
		FormElement.parseInt((node \"@minHeight").text),FormElement.parseInt((node \"@maxHeight").text),(node \"@text").text,
		HorAlign(FormElement.parseInt((node \"@align").text)))
 
  def getTextField(node:xml.Node)= new FormTextField(FormElement.parseInt((node \"@minWidth").text),FormElement.parseInt((node \"@maxWidth").text),
		FormElement.parseInt((node \"@minHeight").text),FormElement.parseInt((node \"@maxHeight").text),
			HorAlign(FormElement.parseInt((node \"@align").text)),(node \"@field").text.toByte)
  
  def getCalcField(node:xml.Node)= new FormCalcField(FormElement.parseInt((node \"@minWidth").text),FormElement.parseInt((node \"@maxWidth").text),
		FormElement.parseInt((node \"@minHeight").text),FormElement.parseInt((node \"@maxHeight").text),
			HorAlign(FormElement.parseInt((node \"@align").text)),StringParser.parse((node \"@ex").text))
}

object Test {
	def main(args:Array[String]) = 
	{
		val l=new FormLabel(0,1,2,3,"hallo",HorAlign.Center )
		println(l.getProps)
		println(l.updateProperty("minWidth", "101").getProps)
	}
}



