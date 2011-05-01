/**
 * Author: Peter Started:08.03.2011
 */
package client.dataviewer.sidePanel
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.{InstanceData, Reference, OwnerReference}
import definition.typ.AllClasses
import javax.swing.table.TableColumn
import definition.expression.StringParser
import definition.typ.NOFORMAT
//import XTabSidePanelModel#Structure

/**
 * 
 */
class XTabColModel (mod:XTabSidePanelModel) {
  
	var colSubsID:Int=_
	var subHeaderSubsID:Int=_
	
	val lock=new Object	
	
	val columnWidth=80
	
	val columnList=collection.mutable.ArrayBuffer[ColumnInfo]()
	
	val colModel=new client.dataviewer.FieldColumnModel
	
	
	
	
	private def clearColModel()= while(colModel.getColumnCount>0)colModel.removeColumn(colModel.getColumn(0))
	
	
	def loadColumns(topParent:Reference,columnPropField:Byte) = lock.synchronized{
		println("load columns"+topParent)		
	  colSubsID=ClientQueryManager.createSubscription(topParent,columnPropField)(columnCallBack)  
	}
	
	
	/** reads the columnHeader instances from the top parent
	 * 
	 */
	private def columnCallBack(action:NotificationType.Value,data:IndexedSeq[InstanceData]):Unit = ClientQueryManager.runInPool  { 
		lock.synchronized {
			println("columnCallBack "+action+" "+data)
			action match {
				case NotificationType.sendData => {
					columnList.clear
					clearColModel
					println("send Col List:"+data)
					for(d<-data)ClientQueryManager.runSw({
						val (col,w)=getColumnForColData(d )
						col.colData =d
					})				
          registerSubHeaderQuery ()         
				}
				case NotificationType.childAdded => {
					ClientQueryManager.runSw(getColumnForColData(data.first))					
					notifyUpdate
				}

				case NotificationType.instanceRemoved => {
					val ix=columnList.findIndexOf(data.first.ref ==_.colData.ref)					
					if(ix>=0 ) { 
						System.out.println("Column Removed found "+ix)
						deleteColumn(ix)
						
						notifyUpdate
						mod.controller .table .peer.repaint()
					}					
				}	
				case NotificationType.FieldChanged => {
					val (col,ix)=getColumnForColData(data.first)					
					col.colData =data.first
					//colModel.getColumnByModelIndex(ix).setHeaderValue(col.toString)
					//notifyUpdate
				}		
			}		
		}
	}
	
	def deleteColumn(ix:Int):Unit= for(s<-mod.structure){
		columnList(ix).shutDown
		columnList.remove(ix)
		//val cellFields=s.numDataCellFields
		val startIx=ix*s.numDataCellFields
		for(i <- 0 until s.numDataCellFields)
		 colModel.removeColumn(colModel.getColumnByModelIndex(startIx+i))
		 for(col <-ix until columnList.size){
			 for(field <-0 until s.numDataCellFields)
				 colModel.getColumnByModelIndex((col+1)*s.numDataCellFields+field).setModelIndex(col*s.numDataCellFields+field)
			 columnList(col).startModelIndex =col*s.numDataCellFields
		 }	
	}
	
	/** checks for updates in subheader objects. The actual objects are loaded in getCellPath
	 * 
	 */
	def registerSubHeaderQuery() = for(s <- mod.structure){		
		subHeaderSubsID=ClientQueryManager.createSubscription(s.yParent,s.yParentPropField) (subHeaderCallBack)
	}
	
	
	private def subHeaderCallBack( action:NotificationType.Value,data:IndexedSeq[InstanceData]):Unit = ClientQueryManager.runInPool  { 
		lock.synchronized {
			println("SubHeader "+subHeaderSubsID+" "+action+" "+data)
			action match {
				case NotificationType.FieldChanged => {
					val ix=columnList.findIndexOf(_.subLevelCellData.ref == data.first.ref)
						if(ix>=0) {
							val colData=columnList(ix)
							colData.setSubLevelCellData (data.first)							
							//notifyColHeaderChanged(ix,colData.toString)							
						}
						else{
							println("Data First:" +data.first.ref+" sublLevelCellData:")
							columnList.foreach(co=> println(co.subLevelCellData+" "+co.subLevelCellData.ref))
							throw new IllegalArgumentException(" can't find sub header data "+data.first.ref+" in column list")					
						}
				}
				case _ => {}
			}
		}
	}	
	
	def getColumnForColData(columnData:InstanceData):(ColumnInfo,Int) = lock.synchronized{
		val ix=columnList.findIndexOf(columnData.ref == _.colData.ref)
		if(ix>=0) (columnList(ix),ix)
		else mod.structure match {
			case Some(s) => {		
				val newColumn=new ColumnInfo(columnData,columnList.size*s.numDataCellFields)
				for(field <-0 until s.numDataCellFields) 
				  colModel.createColumn(columnList.size*s.numDataCellFields+field, newColumn.getColumnName(s,field), columnWidth)				
				columnList+=newColumn			
				//TODO:custom renderer colModel.getColumn(columnList.size-1).setHeaderRenderer(new XRenderer(mod.controller))					
				newColumn.load
				(newColumn,columnList.size-1)
			}
			case None=> throw new IllegalArgumentException("No structure found when try to getColumnForColData "+columnData)	
		}
	}
	
	def shutDown = if(colSubsID!=0) lock.synchronized{
		columnList.foreach(_.shutDown)
		ClientQueryManager.removeSubscription(colSubsID)
		ClientQueryManager.removeSubscription(subHeaderSubsID)
		colSubsID=0
		columnList.clear
	} else println("ColModel subst=0")
	
	def notifyUpdate = lock.synchronized{
		mod.fireTableStructureChanged()
		println("headers:"+columnList.mkString("|"))
	}
	
	def getColumnName(ix:Int):String = lock.synchronized {
		mod.structure match {
			case Some(s) => {
				val column=ix/s.numDataCellFields
				val field=ix % s.numDataCellFields
				columnList(column).getColumnName(s,field)
			}
			case None => ""
		}		
	}
	
	def getColumn(ix:Int)= lock.synchronized {columnList(ix)}
	
	
		
	
	
	/*private def notifyColHeaderChanged(colNr:Int,value:Object)= {
		println("notify header col: "+colNr+" value:" +value)
		colModel.getColumnByModelIndex(colNr).setHeaderValue(value)		
		mod.controller.table.peer.getTableHeader.repaint()
	}*/
	
	
	// **********************************************************************************************************
	
	class ColumnInfo (var colData:InstanceData,var startModelIndex:Int) {
		
	  val cellPath= getCellPath()	
	  var subLevelCellData:InstanceData=_
		var headerObject:Option[InstanceData] = None	
		var headerWords:Array[String]= Array.empty
		val dataCells=collection.mutable.HashMap[Reference,InstanceData]()		
		
		var headerSubstID:Int=_
		var dataSubstID:Int=_
		
		def load=lock. synchronized{
			headerSubstID=ClientQueryManager.createSubscription(colData.ref, mod.findHeaderPropField(colData.ref.typ))(headerCallBack)
			dataSubstID=ClientQueryManager.createSubscription(subLevelCellData.ref, findCellPropField(subLevelCellData.ref.typ))( dataCallBack)
		}	
					
		
	  private def findCellPropField(aType:Int):Byte = {
			val pFields=AllClasses.get.getClassByID(aType).propFields
			for(i <- pFields.indices;val pf=pFields(i))
				if(pf.allowedClass==mod.XTabCellType ) return i.toByte
			throw new IllegalArgumentException("FindCellPropField can't find fitting propfield in class "+aType)
		}	
	  
	  def setSubLevelCellData(newValue:InstanceData)= for(s<-mod.structure){
	  	println("SetSublevelcelldata :"+newValue+" result:"+newValue.resultString+" "+newValue.fieldValue (0))
	  	subLevelCellData=newValue
	  	if( s.subHeaderClass.resultFormat!=NOFORMAT ) // when there is a resultFormat, update the last column
	  	colModel.getColumnByModelIndex(startModelIndex+s.numDataCellFields -1).setHeaderValue(getColumnName(s,s.numDataCellFields-1))
	  	mod.controller.table.peer.getTableHeader.repaint()
	  }
	  
	  def setHeaderObject(newValue:Option[InstanceData]) = for(s<-mod.structure){
	  	headerObject=newValue
	  	headerWords=headerObject match {
	  		case Some(ho)=> ho.toString.split(' ')
	  		case None => Array.empty
	  	}
	  	for(field <-0 until s.numDataCellFields )
	  		colModel.getColumnByModelIndex(startModelIndex+field).setHeaderValue(getColumnName(s,field))
	  }
	  
	  def getHeaderObjWord(wordNr:Int)= {
	  	if(headerWords.size>wordNr)headerWords(wordNr) else ""
	  }
		
		
		private def headerCallBack(notType:NotificationType.Value,data: IndexedSeq[InstanceData]):Unit = 
			ClientQueryManager.runInPool  {lock.synchronized{			
			  if(headerSubstID>0){
			  	println("headerCallBack "+colData+" "+notType+" "+data.mkString)
			  	notType match {
			  		case NotificationType.sendData =>	setHeaderObject(if(data.isEmpty) None else Some(data.first))		  							
			  		case NotificationType.FieldChanged =>	setHeaderObject(if(data.isEmpty) None else Some(data.first))			  		
			  		case NotificationType.instanceRemoved => setHeaderObject(None)			  		
			  		case _ => {}
			  	}		  	
			  }
			}
		}
		
		private def dataCallBack(notType:NotificationType.Value,data: IndexedSeq[InstanceData]):Unit = 
			ClientQueryManager.runInPool  {lock.synchronized{
			  if(dataSubstID>0) {
			  	println("dataCallBack "+dataSubstID+" "+colData+" "+notType+" "+data.mkString)
			  	println(Thread.currentThread)
			  	notType match {
			  		case NotificationType.sendData => {
			  			dataCells.clear
			  			data.foreach(d=> dataCells(d.owners.first.ownerRef ) = d)
			  		}					
			  		case NotificationType.FieldChanged => {
			  			dataCells(data.first.owners.first.ownerRef ) = data.first
			  		}
			  		case NotificationType.instanceRemoved => {
			  			dataCells.find(d=> d._2.ref ==data.first.ref) match {
			  				case Some((owner,data)) => dataCells.remove(owner) 
			  				case None =>
			  			}						
			  		}
			  		/*case NotificationType.childAdded =>{
			  			dataCells(data.first.owners .first.ownerRef)=data.first
			  		}*/
			  		case _ => {}
			  	}			  	
			  	mod.fireTableDataChanged
			  	mod.controller.table.peer.repaint()
			  	//notifyColHeaderChanged(startModelIndex,toString)
			  	//ClientQueryManager.runSw(mod.controller.table.peer.invalidate)
			  	//lock.synchronized{colModel.getColumn(modelIndex).setHeaderValue(toString)}
			  }
			}
		}
		
		
		
		/** Checks if all necessary sublevel cells exist and create missing cells
		 * 
		 */
		private def getCellPath():List[Reference]= lock.synchronized{ 
			var retList=List(colData.ref)
			subLevelCellData=colData // sideeffect !!!
			for (s<-mod.structure){				
				 // if there are only toplevel headers
				for(pathLevel<- s.pathToTopParent.size-2 to 0 by -1 ) {
					val colParentPropField=findCellPropField(retList.head.typ)
					val subCellList=ClientQueryManager.queryInstance(retList.head,colParentPropField )
					subCellList.find(_.owners.first.ownerRef == s.pathToTopParent(pathLevel) ) match {
						case Some(cellInst) => {
							retList=cellInst.ref :: retList
							if (pathLevel==0) subLevelCellData=cellInst 
						}
						case None => { // subcell doesnt exist yet, create
							val yParent=s.pathToTopParent(pathLevel)			  		
							val (allowedType,propField)=mod.getDataCellTypeAndField(yParent.typ)
							val newInst=ClientQueryManager.createInstance(allowedType, Array(new OwnerReference(propField,yParent),
								new OwnerReference(colParentPropField,retList.head)))
							val newRef=	Reference(allowedType,newInst)
							retList=newRef::retList
							if (pathLevel==0) subLevelCellData=ClientQueryManager.queryInstance(newRef, -1).first
						}
					}
				}				
			}
			retList
		}
		
			
		def shutDown= lock.synchronized{
			if(headerSubstID!=0){		
			  ClientQueryManager.removeSubscription(headerSubstID)
			  headerSubstID=0			
		  } else println("ColumnInfo "+headerObject+" "+subLevelCellData+" subst == 0")
		  if(dataSubstID!=0){
		  	ClientQueryManager.removeSubscription(dataSubstID)
		  	dataSubstID=0
		  }
		  dataCells.clear
		}
		
		
		def getDataForYOwner(yOwnerRef:Reference):Option[InstanceData]= lock.synchronized{
			dataCells.get(yOwnerRef)			
		}
		
		
		def setCellValue(yOwnerRef:Reference,field:Byte,value:Object) = {
			var cellRef:Reference= null
			lock.synchronized {for(s<- mod.structure) {
				cellRef= if(dataCells.contains(yOwnerRef))  // change
					dataCells(yOwnerRef).ref							
					else { // create
						val owners=Array(new OwnerReference(s.dataCellPropField,yOwnerRef),
							new OwnerReference(s.dataCellPropFieldInSubHeaders,subLevelCellData.ref))
							val newInst=ClientQueryManager.createInstance(s.dataCellType, owners)						
							new Reference(s.dataCellType,newInst)
					}
			}
			}
		Thread.`yield`()
		lock.synchronized {for(s<- mod.structure) {
			ClientQueryManager.writeInstanceField(cellRef, s.getActualColumnModelIndex(field), StringParser.parse(value.toString))
		}
		}
		}	
		
		
		def getColumnName(s:XTabSidePanelModel#Structure,field:Int):String = lock.synchronized{
			val hw=getHeaderObjWord(field)
			val secondLine= if(field==s.numDataCellFields-1&& s.subHeaderClass.resultFormat!=NOFORMAT )// last field
						 subLevelCellData .resultString else s.dataCellColumns(s.getActualColumnModelIndex(field)).name
			"<HTML><nobr>"+hw+"</nobr><br><nobr>"+secondLine+"</nobr></HTML>"			
		}
		
		
		/*override def toString= "<HTML><nobr>"+(if(headerObject.isDefined) (headerObject.get.toString+"</nobr><br><nobr>") else "[]")+ 
		subLevelCellData.toString +"</nobr></HTML>"*/
	}
}



