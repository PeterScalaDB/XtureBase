/**
 * Author: Peter Started:22.12.2010
 */
package client.print

import javax.swing.table.AbstractTableModel
import definition.expression.{Constant,EMPTY_EX,Expression,StringParser,StringConstant}
import definition.data.{FormDescription,ParameterDescription}
import definition.typ.DataType


/**
 * 
 */
class ParamTabMod extends AbstractTableModel  {
	
	val paramValues=collection.mutable.ArrayBuffer[(String,String,Constant)]()
	var paramDefs:Seq[ParameterDescription]=_
	
	var currentForm:FormDescription= _
	
	var currentParentType:Int = _
	
	def getEmptyValue(name:String)=EMPTY_EX
	
	def loadForm(newForm:FormDescription,getValueFunc:(String)=>Constant= getEmptyValue) = {
		val newParams=newForm.params
		if(paramDefs!=null) {
			// remove unused params in list
			for(op<-paramDefs;if(!newParams.contains(op))){
				val pos=paramValues.findIndexOf(op.name== _._1)
				paramValues.remove(pos)
			}
							
			// add new params
			for(np<-newParams) {
				val ix=paramDefs.findIndexOf(_ == np)
				if(ix<0) paramValues append( (np.name,np.desc,getValueFunc(np.name))) //append value
				else paramValues(ix)=(np.name,np.desc,getValueFunc(np.name)) // change value
			}				
		}	
		else for(np<-newParams) {
			println()
			paramValues append ((np.name,np.desc,getValueFunc(np.name)))
		}
			
		paramDefs=newParams
		fireTableDataChanged()
	}
	
	def getRowCount(): Int = { paramValues.size }

  def getColumnCount(): Int = { 2 }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
  	columnIndex match {
  		case 0 => paramValues(rowIndex)._1 
  		
  		case 1 => paramValues(rowIndex)._2
  		
  		case 2 => paramValues(rowIndex)._3.toString
  	}  
  }
  
  def convertToType(ex:Constant,dtype:Int)= {
  	val t= dtype match {  		
  		case 1 => DataType.IntTyp 
  		case 2 => DataType.DoubleTyp
  		case 3 => DataType.CurrencyTyp
  		case 4 => DataType.StringTyp
  		case 5 => DataType.BoolTyp
  		case 6 => DataType.DateTyp
  		case 7 => DataType.VectorTyp
  		case _ => DataType.undefined 
  	}
  	ex.convertTo(t)
  }
  
  
  override def setValueAt(value:Object,row: Int, col: Int) = if(col==2){
  	val oldV=paramValues(row)
  	val dataType=paramDefs.find(_.name ==oldV._1) match {
  		case Some(pd)=> pd.dataType 
  		case _ => throw new IllegalArgumentException("cant find Datatype for "+oldV._1)
  	}
  	val dat:Constant= if(dataType==4) new StringConstant(value.toString) else convertToType(StringParser.parse(value.toString).getValue,dataType)
  	paramValues(row)=(oldV._1,oldV._2,dat)
  	fireTableDataChanged()
  }
  
  override def getColumnClass(col:Int)= {
  	col match {
  		case 0 => classOf[String]
  		case 1 => classOf[String]
  		case 2 => classOf[String]
  	}
  }
  
  override def isCellEditable(row:Int,col:Int)= {
  	col==2
  }
  
  def getParams:Seq[(String,Constant)]= {
  	paramValues.map(a=>(a._1,a._3))
  }
	
}