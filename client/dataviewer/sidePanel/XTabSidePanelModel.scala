/**
 * Author: Peter Started:08.03.2011
 */
package client.dataviewer.sidePanel
import definition.data.{Reference,OwnerReference}
import definition.typ.AllClasses
import definition.typ.SystemSettings
import client.comm.ClientQueryManager
import javax.swing.table.AbstractTableModel
import definition.data.OwnerReference
import definition.typ.FieldDefinition
import client.dataviewer.TableHeaderMap
import client.dataviewer.InstanceRenderer
import scala.swing.Table
import definition.expression.Expression
import client.dataviewer.EnumRenderer
import definition.typ.ClientObjectClass




/** Data model for XTabSidePanelController
 * 
 */
class XTabSidePanelModel(val controller:XTabSidePanelController) extends AbstractTableModel {
  
	class Structure(parentRef:Reference) {
		val pathToTopParent=findTopParent(parentRef)
		val topParent=pathToTopParent.last
		val (topColumnClass,topParentColPropField)=getColPropFieldForType(topParent.typ)
		val (dataCellType,dataCellPropField)=getDataCellTypeAndField(controller.ydataModel.typ)	
		val dataCellClass=AllClasses.get.getClassByID(dataCellType).asInstanceOf[ClientObjectClass]
		val dataCellColumns:Seq[FieldDefinition]= dataCellClass.fields
		val numDataCellFields=dataCellColumns.size
		val yParent=pathToTopParent.first		
		val (subHeaderType,yParentPropField)=getDataCellTypeAndField(yParent.typ)    
    val dataCellPropFieldInSubHeaders=getDataCellTypeAndField(subHeaderType)._2
    val actualColumnModel=TableHeaderMap.getColumnModel(dataCellType)
    
    lazy val subHeaderClass=AllClasses.get.getClassByID(subHeaderType)
		val instRenderer=new InstanceRenderer(dataCellClass)
		val itcr = new Table.AbstractRenderer[Expression, InstanceRenderer](instRenderer) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int) =     
				component.config(t,sel,foc,o,row,col)
		}
		lazy val etcr = new Table.AbstractRenderer[Tuple2[String,Int], EnumRenderer](new EnumRenderer) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o:Tuple2[String,Int], row: Int, col: Int) = {    
				component.prepare(t,sel,o,row)
    }
		}
		
		private def getColPropFieldForType(aType:Int):(Int,Byte) = {		
			val fieldList=AllClasses.get.getClassByID(aType).propFields
			for(ix<-fieldList.indices;pf=fieldList(ix); if(pf.allowedClass >0)){
				val allowedClass=AllClasses.get.getClassByID(pf.allowedClass)
				if(allowedClass.inheritsFrom(XTabColType)) return (pf.allowedClass,ix.toByte)
			}			
			(-1,-1)
		}

		private def findTopParent (parentRef:Reference):List[Reference] = {
			val (aclass,pfield)=getColPropFieldForType(parentRef.typ )
			if(pfield> -1)  // propfield found, this is the top parent				
				List(parentRef)			
			else { // propfield not found, go one level higher
				val pInst=ClientQueryManager.queryInstance(parentRef,-1).first
				if(pInst.owners .size!=1) throw new IllegalArgumentException("Error: cant find Top Parent, wrong number of owners:"+pInst.owners.size)
				parentRef::findTopParent(pInst.owners.first.ownerRef)  
			}
		}		
		
		/** Allows the reordering of columns in DataCells. 
		 * The reordering info is taken from the TableHeaderMap for the DataCell's type		 * 
		 * 
		 * @param colIx number of visible column
		 * @return model index of reordered Column from TableHeaderMap
		 */
	  def getActualColumnModelIndex(colIx:Int):Byte = {
	  	(actualColumnModel.getColumn(colIx+1).getModelIndex-1).toByte
	  }
		//def numDataCellFields= dataCellColumns.size
	}
	
	val colModel=new XTabColModel(this)	
	
	var structure:Option[Structure]=_
	
	lazy val XTabColType:Int=SystemSettings().systemTypes("XTabCol")
	lazy val XTabCellType:Int=SystemSettings().systemTypes("XTabCell")
	
	
	
	/** gets the Property Field of a certain class aType where the allowed class inherits from XTablColType
	 * 
	 * @param aType classID of a class
	 * @return (ClassID of allowed Class,number propertyField)
	 */	
	def getDataCellTypeAndField(yRowType:Int):(Int,Byte) = {		
		val fieldList=AllClasses.get.getClassByID(yRowType).propFields
		for(ix<-fieldList.indices;pf=fieldList(ix); if(pf.allowedClass >0)){
			val allowedClass=AllClasses.get.getClassByID(pf.allowedClass)
			if(allowedClass.inheritsFrom(XTabCellType)) return (pf.allowedClass,ix.toByte)
		}			
		(0,0)
	}
	
	/* finds the property field in the yParent that contains the links to the headercell
		 * 
		 * @param yParentType type of the yParent
		 * @return (allowed Class,property field)
		 
	def findAllowedSubCellType(yParentType:Int):(Int,Byte) = {
		val pFields=AllClasses.get.getClassByID(yParentType).propFields
		for(i <- pFields.indices;val pf=pFields(i);if(pf.allowedClass>0)){
			//println("check field "+i+" allowed:"+pf.allowedClass+ " "+mod.XTabCellType)
			if(AllClasses.get.getClassByID(pf.allowedClass).inheritsFrom(mod.XTabCellType)) return (pf.allowedClass,i.toByte)
		}
		throw new IllegalArgumentException("FindAllowedSubCellType can't find allowed subclass for cells class "+
			yParentType)	
	}*/
	
	
	def initData(parentRef:Reference) = {
		structure=Some(new Structure(parentRef))				
		println("path:"+structure.get.pathToTopParent)
		
		colModel.loadColumns(structure.get.topParent,structure.get.topParentColPropField.toByte)
	}
	
	def shutDown= {
	  colModel.shutDown
	  structure=None
	}
	
	def notifyRowsChanged = {
		fireTableDataChanged
		controller.mainComp.peer .invalidate
	}
	
	def findHeaderPropField(testClass:Int):Byte = {
		val pFields=AllClasses.get.getClassByID(testClass).propFields
			for(i <- 1 until pFields.size;val pf=pFields(i)) 
				if(pf.allowedClass==0 && pf.single) return i.toByte
			throw new IllegalArgumentException("FindHeaderPropField can't find fitting propfield in class "+		
				testClass)
		}
	
	def addColumn(headerRef:Reference,fromOwner:OwnerReference)= for (s<-structure){
		val inst = ClientQueryManager.createInstance(s.topColumnClass,Array( new OwnerReference(s.topParentColPropField,s.topParent)))		
		
		val headerPropField = findHeaderPropField(s.topColumnClass)
		println("add column new inst:"+inst+" headerField :"+headerPropField+ " headerOBj: "+headerRef+ " fromOwner:" +fromOwner)
		ClientQueryManager.secondUseInstances(List(headerRef), fromOwner, new OwnerReference(headerPropField,new Reference(s.topColumnClass,inst)), -1)
	}
	
	def deleteColumn(ix:Int) = for (s<-structure){
		if(ix % s.numDataCellFields == s.numDataCellFields -1)
		ClientQueryManager.deleteInstance(colModel.getColumn(ix / s.numDataCellFields).colData.ref,new OwnerReference(s.topParentColPropField,s.topParent))
	}
	
		
		
	//******************** Interface TableModel ***************************************************
	
	def getColumnCount= {
		colModel.colModel.getColumnCount
	}
	
	override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
		true
	}

	override def getColumnName(col:Int) = {
		colModel.getColumnName(col)
	}
	
	override def getColumnClass(col:Int):java.lang.Class[_] =  classOf[String]
	
	
	def getRowCount= {
		controller.ydataModel .getRowCount
	}
	
	def getValueAt(row:Int,col:Int)= structure match {
		case Some(s)=> {
			controller.ydataModel.getRowReference(row).flatMap(colModel.getColumn(col/s.numDataCellFields ).getDataForYOwner(_)) match {
				case Some (cellData)=>{
					val mCol=s.getActualColumnModelIndex(  col % s.numDataCellFields)
					val found=if(s.dataCellClass.enumFields==null) -1 else s.dataCellClass.enumFields.findIndexOf(_._1==mCol)
				if(found> -1) s.dataCellClass.enumFields(found)._2.getElem(cellData.fieldValue(mCol).toInt)
				else cellData.fieldData(mCol)
				}
				case None => ""
			}	
		}
		case None =>""
	}
	
	override def setValueAt(value:Object,row:Int,col:Int) = for(s <-structure){
		controller.ydataModel.getRowReference(row).foreach(colModel.getColumn(col/s.numDataCellFields).
			setCellValue(_,(col % s.numDataCellFields).toByte,value))		
	}
	
}