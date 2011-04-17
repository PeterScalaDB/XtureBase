/**
 * Author: Peter Started:05.11.2010
 */
package client.dataviewer

import javax.swing.table._
import definition.typ._
import client.comm._
import scala.collection.JavaConversions._
import client.dialog.ComboBoxEditor
import javax.swing.JComboBox
/** stores the header configuration for all Tables
 * 
 */
object TableHeaderMap {
  val theMap=collection.mutable.HashMap[Int,MyColumnModel]() 
  
  ClientQueryManager.registerStoreSettingsListener(() => {
  	for(entry <-theMap)
  		UserSettings.setListProperty("TableColumns",entry._1.toString,
  		  entry._2.createTupleList)
  })
  
  
  
  def getColumnModel(typ:Int)= if(theMap.contains(typ)) theMap(typ)
  else {
  	val newModel=	setupEmptyColumnModel(typ)
  	theMap(typ)=newModel
  	newModel
  }
  
  def loadColumnModel(typ:Int,setupList:Seq[Tuple2[Int,Int]])= {
  	
  }
  
  
  def setupEmptyColumnModel(typ:Int)= {
  	val newModel=new MyColumnModel
  	val theClass=AllClasses.get.getClassByID(typ).asInstanceOf[ClientObjectClass]
  	val numColumn:Int=theClass.fields.size
  	
  	//val enumEditors:Seq[ComboBoxEditor]=for(ef<-theClass.enumFields) yield new ComboBoxEditor(new JComboBox(ef._2 .javaVect))
  	
  	val firstColumn=new TableColumn(0)
  	firstColumn.setHeaderValue(" ")
  	firstColumn.setMaxWidth(25)
		firstColumn.setPreferredWidth(25)
		firstColumn.setResizable(false)
		
		newModel.addColumn(firstColumn)
		// is there a stored TableSetup for this type ?
		val settings=UserSettings.getListProperty[Tuple2[Int,Int]]("TableColumns",typ.toString,Nil)
		if(!settings.isEmpty) {
			System.out.println("settings for Typ:"+typ+" "+settings+" ")
			for(cs <-settings){				
				val newCol=new TableColumn(cs._1)
				try {
					newCol.setHeaderValue(theClass.fields(cs._1-1).name)					
					newCol.setWidth(cs._2)
					newCol.setPreferredWidth(cs._2)
					if(theClass.enumFields!=null) theClass.enumFields find(_._1 ==cs._1-1) match {
						case Some(tuple)=> newCol.setCellEditor(new ComboBoxEditor(new JComboBox(tuple._2 .javaVect)))
						case None =>
					}
				} catch {case e => System.err.println(e) } 
				newModel.addColumn(newCol)
			}
		} else
		for(i <-0 until numColumn){
			val newCol=new TableColumn(i+1)
			newCol.setHeaderValue(theClass.fields(i).name)
			newModel.addColumn(newCol)
		}
  	/*if(theClass.enumFields!=null)
  	for(ef<-theClass.enumFields){
  		println("column "+ef._1+" set editor "+ef._2 .javaVect .mkString(","))
  		newModel.getColumn(ef._1+1).setCellEditor(new ComboBoxEditor(new JComboBox(ef._2 .javaVect)))
  	}*/
		newModel	
  }
}

class MyColumnModel extends DefaultTableColumnModel {
	def createTupleList={	
		for(i<-1 until tableColumns.size;col=tableColumns.get(i))
		  yield (col.getModelIndex,col.getWidth)
	}
}