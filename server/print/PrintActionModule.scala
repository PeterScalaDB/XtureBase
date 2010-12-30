/**
 * Author: Peter Started:19.12.2010
 */
package server.print

import server.storage.{ActionModule,ActionImpl,ActionIterator,StorageManager,ActionNameMap}
import definition.data._
import definition.typ._
import definition.expression.{Constant,IntConstant,StringConstant}
import transaction.handling.{TransactionManager,SessionManager,ActionList}
import server.comm.UserSocket
import server.storage.StorageManager

/**
 * 
 */
class PrintActionModule extends ActionModule {
	var outDefType:Int= _ 
	var paramValueType:Int = _
	
	
	SessionManager.registerSetupListener(setup)
	
	
	val outputAction=new ActionIterator("Ausgabe",None,doOutputWindow)
	
	val mList=List(outputAction)
	
	def setup() = {
		outDefType= SystemSettings().systemTypes("OutputDef")
		paramValueType= SystemSettings().systemTypes("ParamValue")
		PrintGenerator.printableType=SystemSettings().systemTypes("Printable")
		//todo: get printable type !!!
	}
	
	def doOutputWindow(u:UserSocket,parent:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean = {
		if(data.isEmpty)return false
		if(!PrintFormsHandler.typesList.contains(data.first.ref.typ)) {
			System.err.println("Cant find forms for type: "+data.first.ref.typ)
			return false
		}
		val formsList=PrintFormsHandler.typesList(data.first.ref.typ)
		println("Printforms :"+formsList.mkString("\n"))
		val outDefs= StorageManager.getInstanceProperties(data.first.ref) match {
			/*case Some(data) => data.propertyFields (0).propertyList .filter(_.typ ==outDefType).
			map(a => OutputDefinition(StorageManager.getInstanceData(a)).toXML)*/
			case Some(pdata)=> for(p <-pdata.propertyFields(0).propertyList;if(p.typ==outDefType))				
				yield  {
				  val paramList:Seq[InstanceData]=StorageManager.getInstanceProperties(p) match {
				  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData(_))
				  	case None => Seq.empty
				  }
					OutputDefinition( StorageManager.getInstanceData(p),paramList ).toXML
				}			
			case None => Seq.empty
		}
		
		val question=new CustomQuestion("client.print.PrintQuestionHandler", 
			<forms> {formsList.map(_.toXML)} </forms>
		  <outDefs> {outDefs}</outDefs>
			)
		u.askEnquiry(question,selectOutputDefFunc)
  	//}  		
  	
  	
  	def selectOutputDefFunc(u:UserSocket,returnData:Seq[(String,Constant)]):Unit= {
			println("select:"+returnData.mkString(","))
			if(returnData.isEmpty) return
			var printForm:FormDescription = null
			returnData.first._1 match {
				case "NewOutDef" => { // new definition
					val formNumber=returnData.first._2.toInt
					printForm=formsList(formNumber)
					val printer=returnData(1)._2
					val pageSettings=returnData(2)._2
					val portrait=returnData(3)._2
					val pageWidth=returnData(4)._2.toInt
					val pageHeight=returnData(5)._2.toInt
					TransactionManager.doTransaction(u.userEntry.info.id, ActionNameMap.getActionID("AusgabeDef erzeugen"),data.first.ref, false, outDefType,{
					  var outDefInst=TransactionManager.tryCreateInstance(outDefType, Array(new OwnerReference(0,data.first.ref)), true, -1, true, true)
					  outDefInst=outDefInst.setField(0,IntConstant(printForm.inst))
					  outDefInst=outDefInst.setField(1,printer)
					  outDefInst=outDefInst.setField(2,pageSettings)
					  outDefInst=outDefInst.setField(3,portrait)					  
					  TransactionManager.tryWriteInstanceData(outDefInst)
					  val paramList=returnData.drop(6)
					  println("plist:"+paramList+" outdefInst="+outDefInst)
					  val childList= for(p <-paramList) yield {
					  	println("add param"+p)
					  	var pInst=TransactionManager.tryCreateInstance(paramValueType, Array(new OwnerReference(0,outDefInst.ref)),true)
					  	println("pinst:"+pInst)
					  	pInst=pInst.setField(0,StringConstant(p._1))
					  	pInst=pInst.setField(1,p._2)
					  	TransactionManager.tryWriteInstanceData(pInst)
					  	pInst
					  }	
					  PrintGenerator.generatePages(u,data.first,OutputDefinition(outDefInst,childList),pageWidth,pageHeight,printForm)
					})
					
				}
				case "DeleteOutDef" => { // delete definition
					val outDefInst=returnData.first._2.toInt
					val outDefRef=new Reference(outDefType,outDefInst)
					TransactionManager.doTransaction(u.userEntry.info.id, ActionNameMap.getActionID("AusgabeDef löschen"),outDefRef, false, -1,{
						TransactionManager.tryDeleteInstance(outDefRef,Some(new OwnerReference(0,data.first.ref)),None)
					})
				}
				case "ChoseOutDef" => { // delete definition
					val outDefInstID=returnData.first._2.toInt
					val pageWidth=returnData(1)._2.toInt
					val pageHeight=returnData(2)._2.toInt
					val outDefRef=new Reference(outDefType,outDefInstID)
					val outDefInst=ActionList.getInstanceData(outDefRef)
					val paramList:Seq[InstanceData]=ActionList.getInstanceProperties(outDefRef) match {
					  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData(_))
					  	case None => Seq.empty
					  }	
					val outDef=OutputDefinition(outDefInst,paramList)
					val printForm= PrintFormsHandler.getForm(data.first.ref.typ,outDef.formInst )
					PrintGenerator.generatePages(u,data.first,outDef,pageWidth,pageHeight,printForm)
				}
				
				case "ChangeOutDef" => { // new definition
					val odInst=returnData.first._2.toInt
					val formNumber=returnData(1)._2.toInt
					printForm=formsList(formNumber)
					val printer=returnData(2)._2
					val pageSettings=returnData(3)._2
					val portrait=returnData(4)._2
					val pageWidth=returnData(5)._2.toInt
					val pageHeight=returnData(6)._2.toInt
					val odRef=new Reference(outDefType,odInst)
					TransactionManager.doTransaction(u.userEntry.info.id, ActionNameMap.getActionID("AusgabeDef ändern"),data.first.ref, false, -1,{
					  var outDefInst=ActionList.getInstanceData(odRef)
					  outDefInst=outDefInst.setField(0,IntConstant(printForm.inst))
					  outDefInst=outDefInst.setField(1,printer)
					  outDefInst=outDefInst.setField(2,pageSettings)
					  outDefInst=outDefInst.setField(3,portrait)					  
					  TransactionManager.tryWriteInstanceData(outDefInst)
					  val paramList=returnData.drop(7)
					  println("plist:"+paramList+" outdefInst="+outDefInst)
					  // load existing param objects
					  val oldParamList:Seq[InstanceData]=StorageManager.getInstanceProperties(odRef) match {
					  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData(_))
					  	case None => Seq.empty
					  }					  
					  val childList = for(p <-paramList) yield {
					  	println("write param"+p)					  	
					  	var pInst= oldParamList.find(_.fieldValue(0).toString==p._1) match {
					  		case Some(instDat)=>instDat
					  		case None =>TransactionManager.tryCreateInstance(paramValueType, Array(new OwnerReference(0,outDefInst.ref)),true)
					  	}
					  	println("pinst:"+pInst)
					  	pInst=pInst.setField(0,StringConstant(p._1))
					  	pInst=pInst.setField(1,p._2)
					  	TransactionManager.tryWriteInstanceData(pInst)
					  	pInst
					  }	
					  PrintGenerator.generatePages(u,data.first,OutputDefinition(outDefInst,childList),pageWidth,pageHeight,printForm)
					})
					
				}
			}
		}
		true
  }
	
  def getActionsIterator = mList.iterator
  
  def setObjectType(typeID:Int)={}
  
  
  
  

}