/**
 * Author: Peter Started:24.11.2010
 */
package client.dataviewer

import definition.data._
import definition.typ._
import definition.expression._
import scala.swing._
import java.awt.{Color}
import javax.swing.{UIManager,JLabel}

/**
 * 
 */
class InstanceRenderer(theClass:AbstractObjectClass) extends Label {
	override lazy val peer: JLabel = 
    new JLabel("", null, Alignment.Left.id) with SuperMixin {
		override def validate()= {}
		override def invalidate() = {}
	}
	
	val alternateColor=new Color(250,251,254)
	val m=new javax.swing. table.DefaultTableCellRenderer
	val linkColor=new Color(0,150,0)
	val nofocusBorder=UIManager.getBorder("Table.cellNoFocusBorder")
	val focusBorder=UIManager.getBorder("Table.focusCellHighlightBorder")
	val focusForeground = UIManager.getColor("Table.focusCellForeground");
	val focusBackground = UIManager.getColor("Table.focusCellBackground");
	
	def config(t:Table, isSelected: Boolean, focused: Boolean, expression: Expression,row:Int, col: Int) :Unit= {
		if (focused)  border= focusBorder
		else border=nofocusBorder
		
  	background=if(isSelected)  t.selectionBackground 
  	  else  if (row % 2 == 0)alternateColor 
  	  			else Color.white
		
		//print("ex:"+expression+" row:"+row+" col:"+col)
  	if(expression== null || expression.isNullConstant) {
  		text=""  		
  	}
  	else {
  		val fieldFormat=theClass.fieldSetting(col-1)

  		if (fieldFormat.showFormula) {	
  			text=expression.getTerm
  			this.horizontalAlignment=Alignment.Left
  			foreground=Color.black
  		}
  		else { // show value
  			val fieldDef=theClass.fields(col-1)
  			val value=expression.getValue  		
  			import DataType._
  			text = if(value==null) "" else (fieldDef.typ match {
  				case IntTyp | LongTyp => {
  					horizontalAlignment=Alignment.Right  				
  					value.toLong.toString 
  				}
  				case DoubleTyp|CurrencyTyp => {
  					horizontalAlignment=Alignment.Right
  					if(fieldFormat.formString .length>0) fieldFormat.formString .format(value.toDouble)
  					else value.toDouble.toString  				
  				}
  				case BoolTyp => {
  					horizontalAlignment=Alignment.Right
  					value.toBoolean.toString  				
  				}
  				case StringTyp => {
  					horizontalAlignment=Alignment.Left
  					value.toString

  				}
  				case VectorTyp => {
  					horizontalAlignment=Alignment.Left
  					value.toVector.shortToString  				
  				}
  				case _ => {
  					horizontalAlignment=Alignment.Center
  					value.toString  				
  				}
  			}).replaceAll("\n"," | ")
  			foreground= if(isSelected) t.selectionForeground
  			else expression.getType match {  			
  				case BinOp |CollFunctionCall |FunctionCall => Color.blue
  				case FieldRefTyp => linkColor
  				case _ => Color.black  			
  			}
  		}
  		if (focused) {          
  			if (!isSelected ) {                        
  				foreground=focusForeground
  				background=focusBackground                        
  			}
  		}
  	}  	  
	}	
	
}