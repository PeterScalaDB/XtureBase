/**
 * Author: Peter Started:21.04.2011
 */
package client.dataviewer
import definition.typ.form._
import definition.typ.AbstractObjectClass
import definition.data.Reference
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.InstanceData
import definition.expression.Expression
import definition.expression.EMPTY_EX
import definition.typ.DataType
import definition.expression.StringParser
import definition.expression.StringConstant
import server.test.SimpleProfiler
/**
 * 
 */
class FormModel extends DataChangeListener {
	var theClass:AbstractObjectClass = _
	var forms:Option[FormBox]=None
	var dataRef:Reference = _
	var dataValue:InstanceData = _
	
	var bestID:Int= _
	
	val lock = new Object
	
	def setForms(nclass:AbstractObjectClass,nform:Option[FormBox])= lock.synchronized{
		shutDown()
		theClass=nclass		
		forms=nform
		for (f<-forms) f.setListener(Some(this))			
	}
	
	def loadData(nref:Reference) = {
		shutDown()
		SimpleProfiler.measure("FormModel loadData")
		if(forms.isDefined) {
			if(nref.typ != theClass.id) throw new IllegalArgumentException("FormModel Wrong dataType "+nref+ " expected:"+theClass.name)
			dataRef=nref
			bestID=ClientQueryManager.createSubscription(nref, -1) ((n:NotificationType.Value,data:IndexedSeq[InstanceData]) => 
			ClientQueryManager.runInPool( lock.synchronized{
				n match {
					case NotificationType.FieldChanged | NotificationType.sendData  => setDataValue(data.first) 
					case NotificationType.instanceRemoved => shutDown
				}
			} ))		
		}
	}
	
	def setDataValue(newValue:InstanceData) = lock.synchronized{ 
		for(f<-forms) {
			//println("setDataValue: "+newValue)
			SimpleProfiler.measure("FormModel setDataValue B")
			f.setDataValue(newValue)
			SimpleProfiler.measure("FormModel setDataValue A")
		}
	}
	
	def shutDown() = if(bestID>0)lock.synchronized{		
		for (f<-forms;el<-f) el match{
			case de:FormDataField=>de.wantShutDown;de.shutDown
			case _ =>
		}
		ClientQueryManager.removeSubscription(bestID)
		bestID=0	
	}
	
	// interface DataChangelistener ************************************
	
	def fieldChanged(field:Byte,newValue:Expression) = lock.synchronized{
		//println("field changed :"+dataRef+" field:"+field+" value:"+newValue.getTerm)
		if(bestID==0) throw new IllegalArgumentException("FormModel fieldchanged after shutDown: "+dataRef+" field:"+field+" value: "+newValue)
		ClientQueryManager.writeInstanceField(dataRef, field, newValue)
	}
	
	def parseValue(fieldNr:Byte,text:String):Expression = lock.synchronized{		
		if(text.length==0)Expression.generateNullConstant(theClass.fields(fieldNr).typ) else
			if (theClass.fields(fieldNr).typ==DataType.StringTyp)
				try {
					StringParser.parse( text) 
				} 
		catch {
			case _ => new StringConstant(text)				
		}
		else StringParser.parse( text) // throw exception when fail
	}
		
}