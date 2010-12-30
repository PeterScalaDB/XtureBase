/**
 * Author: Peter Started:28.11.2010
 */
package management.databrowser

import javax.swing.table._
import definition.typ._
import definition.data._
import definition.expression._
import server.storage.ServerObjectClass

/**
 * 
 */
abstract class ActivableAbstractTableModel extends AbstractTableModel {	
	var isDirty:Boolean=false
	
	protected var _isCreating:Boolean=false
	def isCreating_=(newValue:Boolean)= {
		_isCreating=newValue
		fireTableDataChanged
	}
	def isCreating=_isCreating 
	
	def update(theClass:ServerObjectClass) 
}



class FieldDefTableModel(showLastLine:Boolean) extends ActivableAbstractTableModel {
	
  var fieldDefList:Seq[FieldDefinition]=Seq.empty
  var fieldSettingList:Seq[FieldSetting]=Seq.empty
  
  def getRowCount(): Int = fieldDefList.size+( if(showLastLine)1 else 0)

  def getColumnCount(): Int = { 8 }
  
    
  override def getColumnClass(col:Int):Class[_] = {
  	col match {  
  		case 0 => classOf[String]
  		case 1 => classOf[DTWrap]
  		case 2 => classOf[EnumDefinition]
  		case 3 => classOf[Boolean]
  		case 4 => classOf[Boolean]
  		case _ => classOf[String]
  	}
  }
  
  override def isCellEditable(row:Int,col:Int):Boolean = {
  	if (col>1) true
  	else if(_isCreating) showLastLine
  	else false    	
  }

  def getValueAt(row: Int, column: Int): Object = {
  	if(row>=fieldDefList.size) null
  	else {
  		if(column<3) {
  		 val fd=fieldDefList(row)  		 
  		 column match {
  			 case 0 => fd.name 
  			 case 1 => DataType.wrapMap(fd.typ).asInstanceOf[AnyRef]
  			 case 2 => fd match {
  				 case f:EnumFieldDefinition => SystemSettings().enumByID( f.enumID)
  				 case _ => NOENUM
  			 }
  		 }
  		}
  		else {
  			val fs=fieldSettingList(row)
  			column match {
  				case 3 => fs.readOnly .asInstanceOf[AnyRef]
  				case 4 => fs.showFormula .asInstanceOf[AnyRef]
  				case 5 => fs.editor
  				case 6 => fs.startValue.getTerm
  				case 7 => fs.formString
  				case _ => null
  			}
  		}
  	}  	 
  }
  
  override def setValueAt(value:Object,row:Int,column:Int):Unit= {  	
  	if(row==fieldDefList.size&&showLastLine) {
  		// adding field  		
  		val newField =new FieldDefinition("",DataType.undefined )
  		fieldDefList=fieldDefList:+newField
  		fieldSettingList=fieldSettingList:+new FieldSetting(fieldDefList.size-1)
  	}  	
  	if(column<3 && (  !showLastLine))return
  	if(column==1 && !_isCreating) return
  	if(column<3) {
  	  var field:FieldDefinition=fieldDefList(row) 
  	  //println("Mod set value:"+value+" col:"+column)
  	  fieldDefList=MainWindow.updateSeq(fieldDefList,row,
  	  	column match {
  	  	case 0 =>field.setName(value.toString)
  	  	case 1 => field.setType(value.asInstanceOf[DTWrap].typ)
  	  	case _ =>field.setEnumID(value.asInstanceOf[EnumDefinition].id) 
  	  })
  	  	  	  	  
  	}
  	else {
  		val fs= {
  			val w= fieldSettingList(row)  		
  			if(w.fieldNr == -1) {
  				val n=new FieldSetting(row)
  				fieldSettingList=MainWindow.updateSeq(fieldSettingList,row,n)
  				n
  			}
  			else w
  		}
  		column match {
  	    case 3 => fs.readOnly=value.asInstanceOf[Boolean].booleanValue
  	    case 4 => fs.showFormula=value.asInstanceOf[Boolean].booleanValue
  	    case 5 => fs.editor=value.toString
  	    case 6 => fs.startValue=StringParser.parse(value.toString)
  	    case 7 => fs.formString=value.toString
  	    case _ => null		
  		}
  	}
  	isDirty=true 
  	//System.out.println("set Value ready "+fieldDefList.mkString(","))
  	fireTableDataChanged
  }

  def setValues(newDefs:Seq[FieldDefinition],newSettings:Seq[FieldSetting]):Unit= {
  	fieldDefList=newDefs
  	fieldSettingList=newSettings
  	isDirty=false
  	super.fireTableDataChanged()
  }
  
  def update(theClass:ServerObjectClass) = if(isDirty){
  	if(showLastLine)
  		theClass.ownFields=fieldDefList
  	theClass.ownFieldSettings=fieldSettingList.filter(_.fieldNr> -1)
  }
}