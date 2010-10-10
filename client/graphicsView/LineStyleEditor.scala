/**
 * Author: Peter Started:10.10.2010
 */
package client.graphicsView

import definition.typ._
import scala.swing._
import scala.swing.event._
import definition.data._
import client.comm._
import definition.expression.IntConstant

/**
 * 
 */



class LineStyleEditor extends FieldEditor {
	
	val widthList=List(0.1,0.15,0.2,0.25,0.35,0.5,0.7)
	val combo=new ComboBox(widthList)
	
	val fieldNr=1.toByte
	val allowedClasses=List(40,41)
	
	var dataList:Seq[Referencable]=Seq.empty	
	
	
	var selfSelected=false
	
	val panel=new GridPanel(1,2) {
		contents +=new Label("Dicke:")
		contents +=combo
		preferredSize=new Dimension(70,32)
		listenTo(combo.selection)
		reactions+={
			case e:SelectionChanged=> if(selfSelected) selfSelected=false
			else if(combo.selection.index> -1){
				val dat:Double=combo.selection.item									
				val lineWidth:Int=(dat*100).toInt
				println("change width:"+ lineWidth+" in "+dataList.mkString)
				if(dataList!=null){
					val sendList=dataList.filter(inst =>allowedClasses.contains(inst.ref.typ ))
					if(!sendList.isEmpty)
						ClientQueryManager.writeInstancesField(sendList,fieldNr,new IntConstant(lineWidth))
				}				
			}
		}
		
	}
	
  def getPanel:Panel=panel
	def setData(data:Seq[Referencable])= {
    dataList=data
    if(data!=null) {
      var lineWidth:Int= -2
      if(!data.isEmpty && data.first.isInstanceOf[GraphElem]) 
      	for(el <-data;if(el.isInstanceOf[LinearElement])) {
      		val d=el.asInstanceOf[LinearElement]
      		if(lineWidth!= -1) {
      		  if(lineWidth== -2)lineWidth=d.lineWidth 
      		  else if(lineWidth!=d.lineWidth) lineWidth= -1	
      		}      		
      	}
      if(lineWidth<0)combo.selection.index= -1
      else {
      	val newWidth=lineWidth.toDouble/100
      	selfSelected=true
      	combo.selection.index=widthList.indexOf(newWidth)
      }
    }    
    
  }
}