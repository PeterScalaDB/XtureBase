/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ.form

import scala.swing.BoxPanel
import scala.xml.Node
import scala.swing.Orientation
import scala.swing.Component
import definition.data.InstanceData

/** trait for accessing the addBox of the Designer
 * 
 */
trait SpecialComponent extends Component {
	var formBox:FormBox
}
/**
 * 
 */
case class FormBox(val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val orient:Orientation.Value,val elements:Seq[FormElement],var specialComp:Option[SpecialComponent]=None) extends BoxPanel(orient) with FormElement {
	
	background=FormElement.formColor
	setupComponent(this)
	elements.foreach(_ match {
		case e:Component=> contents+=e 
		case other => println("unknown type:"+other)
	})
	for(c<-specialComp) {
		contents+=c
		c.formBox=this
	}

  def toXML(): Node = { 
  	<FormBox minWidth={minWidth.toString} maxWidth={maxWidth.toString} minHeight={minHeight.toString} maxHeight={maxHeight.toString} orient={orient.id.toString}>
    {elements.map(_.toXML)}
  	</FormBox>
  }
  
  def addElement(newElement:FormElement,pos:Int= -1)={
  	val nb=new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,
  		if(pos < 0)	elements:+newElement
  		else (elements.take(pos):+newElement)++elements.drop(pos)
  		,specialComp)
  	
  	for( c<-specialComp) c.formBox=nb
  	nb
  }
  
  def setSpecialComp(s:SpecialComponent)= {
  	specialComp=Some(s)
  	s.formBox=this
  	contents+=s
  }
  
  def makeCopy ={   	
  	new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.map(_.makeCopy))
  }
  
  def updateElement(pos:Int,newElement:FormElement)= {
  	val nb=new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.updated(pos,newElement),specialComp)
  	for( c<-specialComp) c.formBox=nb
    /*for(i<-0 until this.contents.size)  {	
    	println("C "+this.contents.mkString(","))
    	if(i==pos) {nb.contents+=newElement.asInstanceOf[Component];this.contents.remove(0)}
    	else nb.contents+=this.contents.first
    }*/
  	nb
  }
  
  def updateElement(oldValue:FormElement,newValue:FormElement):(FormBox,Boolean) = {
  	//println("updateElement oldValue:"+oldValue+" newValue "+newValue)
  	if(oldValue eq this)  return(newValue.asInstanceOf[FormBox],true)
  	for(ix <-elements.indices;el=elements(ix) ) {
  		if(el eq oldValue) return (updateElement(ix,newValue),true)
  		el match {
  			case fb:FormBox => {
  				val nfb=fb.updateElement(oldValue,newValue)
  				if(nfb._2) return (updateElement(ix,nfb._1),true)
  			}
  			case _ =>
  		}  		
  	}
  	return (this,false)
  }
  
  def deleteElement(elem:FormElement):(FormBox,Boolean) = {
  	//println("Delete Element "+elem+" from box:"+toString)
  	for(ix <-elements.indices;el=elements(ix) ) {
  		if(el eq elem) 
  			return (new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.filterNot(_ eq elem),specialComp),true)
  		el match {  			
  			case fb:FormBox => {
  				val nfb=fb.deleteElement(elem)
  				if(nfb._2) return (updateElement(ix,nfb._1),true)
  			}
  			case _ => 
  		}
  	}
  	(this,false)
  }
  
  override def toString = "(FormBox "+orient+" Elems:"+elements.mkString(" , ")+" )"
  
  
  def foreach(s:(FormElement)=>Unit):Unit= {
  	s(this)
  	elements.foreach(_ match {
  		case fb:FormBox =>fb.foreach(s)
  		case other => s(other)
  	})
  }
  
  def setDataValue(dvalue:InstanceData):Unit= elements.foreach (_ match {
  	case df:FormDataField=> df.setDataValue(dvalue)
  	case fb:FormBox => fb.setDataValue(dvalue)
  	case _ =>
  })
  
  def setListener(nlist:Option[DataChangeListener]):Unit = elements.foreach (_ match {
  	case df:FormDataField=> df.setListener(nlist)
  	case fb:FormBox => fb.setListener(nlist)
  	case _ =>
  })
  
  /** finds the formbox that contains the given element
   * 
   * @param el the element 
   * @return (the containing Formbox, index of element in Formbox)
   */
  def findFormBoxFor(el:FormElement):(FormBox,Int)= {
  	val ix=elements.findIndexOf(_ eq el)
  	if(ix> -1) return (this,ix)
  	
  	for (e <-elements) e match {
  		case fb:FormBox =>{
  			val ret=fb.findFormBoxFor(el)
  			if(ret._2> -1) return ret
  		}
  		case _ =>
  	}
  	(null,-1)
  }
}

object FormBox {
	def apply(node:Node):FormBox = {
		new FormBox(FormElement.parseInt((node \"@minWidth").text),FormElement.parseInt((node \"@maxWidth").text),
		FormElement.parseInt((node \"@minHeight").text),FormElement.parseInt((node \"@maxHeight").text)
			,Orientation(FormElement.parseInt((node \"@orient").text)),FormElement.readElements(node))
	}
	
	
}