/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ.form

import scala.swing.TextField
import scala.xml.Node
import definition.data.InstanceData
import definition.typ.HorAlign
import definition.data.Reference
import definition.typ.DataType
import scala.swing.event.EditDone
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import java.awt.Color
import scala.swing.event.FocusGained
import definition.expression.Expression

/**
 * 
 */
class FormTextField (val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val align:HorAlign.Value,val fieldNr:Byte) extends TextField with FormDataField {
  //val editColor=new Color(130,55,55)
  val editColor=new Color(255,255,230)
  var oldText:String=_
  var shuttedDown=false
	horizontalAlignment=HorAlign.toScalaAlignment(align)
	var expr:Expression=_
	
	var dirty:Boolean=false	
	setupComponent(this)
	listenTo(this)
	
	peer.getDocument.addDocumentListener(new DocumentListener {
		def insertUpdate(e:DocumentEvent ) = if(!dirty) setDirty(true)
    def removeUpdate(e:DocumentEvent ) = if(!dirty) setDirty(true)
    def changedUpdate(e:DocumentEvent )= if(!dirty) setDirty(true)
	})
	
	def setDirty(newValue:Boolean)= {
		dirty=newValue
		background= if(dirty)editColor else Color.white
	}
	
	reactions += {
		case e:EditDone=> 
			if(!shuttedDown)
				for (l<-listener) 
					if(text!=oldText)
						l.fieldChanged(fieldNr, l.parseValue(fieldNr, text))
		
		case f:FocusGained => if(!shuttedDown&&expr!=null) {
			text= if(expr.getType==DataType.StringTyp)expr.toString else expr.getTerm
			setDirty(false)
		}
	}
			
  def wantShutDown(): Unit = { 
  	if (dirty)for(l<-listener){
  		l.fieldChanged( fieldNr, l.parseValue(fieldNr,text))
  	}
  }

  def shutDown(): Unit = {  
  	text=""
  	setDirty(false)
  	shuttedDown=true
  }

  def toXML(): Node = {  
  	<TextField minWidth={minWidth.toString} maxWidth={maxWidth.toString} minHeight={minHeight.toString} maxHeight={maxHeight.toString} align={align.id.toString} field={fieldNr.toString}/>
  	
  }
  
  def setDataValue(dvalue:InstanceData) = {  	
  	expr=dvalue.fieldData(fieldNr)
  	//println("TextField field:"+fieldNr+" dataValue: "+ expr)
  	oldText=if(expr.getType==DataType.StringTyp)expr.toString
  	  else expr.getTerm
  	text =expr.getValue.toString  	
  	setDirty(false)
  }
  
  def makeCopy=new FormTextField(minWidth,maxWidth,minHeight,maxHeight,align,fieldNr)
  
  override def toString = "TextField F"+fieldNr
}