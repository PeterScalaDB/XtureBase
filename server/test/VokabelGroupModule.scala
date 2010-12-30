/**
 * Author: Peter Started:14.12.2010
 */
package server.test

import definition.typ._
import server.storage._
import definition.data._
import definition.expression._
import server.comm.UserSocket
import transaction.handling.TransactionManager
import definition.comm.ClientCommands


/**
 * 
 */
class VokabelGroupModule extends ActionModule {
  
 
  var vokabelTyp= -1
  
  val lernAlleDeutsch=new ActionImpl("Lerne Deutsch",None,doLerneDeutsch(0,true))
  val vertiefeDeutsch=new ActionImpl("Vertiefe Deutsch",None,doLerneDeutsch(1,true))
  val lernAlleEnglisch=new ActionImpl("Lerne Englisch",None,doLerneDeutsch(0,false))
  val vertiefeEnglisch=new ActionImpl("Vertiefe Englisch",None,doLerneDeutsch(1,false))
  
  val aQuestion=new DialogQuestion("was heisst",Seq(new ParamAnswerDefinition("Was Heisst :",DataType.StringTyp,None)))
  
  val actionList=List(lernAlleDeutsch,vertiefeDeutsch,lernAlleEnglisch,vertiefeEnglisch)
  
  def getActionsIterator() = actionList.iterator
  
  def setObjectType(typeID: Int): Unit = {}
  
  def init()= {
  	if(vokabelTyp== -1)vokabelTyp =AllClasses.get.getClassIDByName("Vokabel")
  }
  
  
  
  def doLerneDeutsch(nurLevel:Int,de_Richtung:Boolean)(u:UserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {  	
  	init()
  	var vokListe=StorageManager.loadChildren(data.ref,vokabelTyp,0)
		System.out.println("lerne Deutsch "+data)
		
		def getNextDeutschWert():String = {
  		if(vokListe.isEmpty) return ""
  		var dw=vokListe.first.fieldValue(if(de_Richtung)0 else 1).toString.trim
  		var level=vokListe.first.fieldValue(if(de_Richtung)2 else 3).toInt
  		while((dw.size==0||level<nurLevel)&& vokListe.size>1) {
  			vokListe=vokListe.drop(1)
  			dw=vokListe.first.fieldValue(if(de_Richtung)0 else 1).toString.trim
  			level=vokListe.first.fieldValue(if(de_Richtung)2 else 3).toInt
  		}
  		if(level<nurLevel)"" else dw
  	}
		
		if(vokListe.isEmpty) return true
		var deutschWert=getNextDeutschWert()
  	if(deutschWert.size==0) return true
  	
		val question= new DialogQuestion("Vokabeln lernen. Bitte eingeben ->",Seq(new ParamAnswerDefinition("Was heisst '"+deutschWert+"' ?",DataType.StringTyp,None)))
		u.askEnquiry(question,handleAnswer)
		
		def handleAnswer(u:UserSocket,params:Seq[(String,Constant)]):Unit= {
  		if(params.size==0)System.out.println("Stop")
			else {
				val answer=params(0)._2 .toString
				System.out.println("frage:"+deutschWert+" answer:"+answer)
				val rightAnswer=vokListe.first.fieldValue(if(de_Richtung)1 else 0).toString.trim.toLowerCase
				val level=vokListe.first.fieldValue(if(de_Richtung)2 else 3).toInt
				val response = 
				if(answer.trim.toLowerCase==rightAnswer) {
					if (level>0) writeLevel(u,vokListe.first.ref,2,level-1)
					"Das ist RICHTIG ! \n"					 
				}
				else { 
					writeLevel(u,vokListe.first.ref,if(de_Richtung)2 else 3,level+2)
					"'"+answer+"' ist falsch, es heisst: '"+rightAnswer+"'\n"
				}
				vokListe=vokListe.drop(1)
				deutschWert=getNextDeutschWert
				if(deutschWert.size==0){
					u.askEnquiry(new DialogQuestion(response,Seq(new ParamAnswerDefinition("Alle Vokabeln sind durch ! Gut ?",DataType.BoolTyp,None))),
						(a,b)=>{})
				}
				else {
					val question= new DialogQuestion(response,Seq(new ParamAnswerDefinition("Was heisst '"+deutschWert+"' ?",DataType.StringTyp,None)))
					u.askEnquiry(question,handleAnswer)
				}
			}
  	}		
		
		true
	}
  
  def writeLevel(user:UserSocket,ref:Reference,fieldNr:Byte,newValue:Int)= {
  	TransactionManager.doTransaction(user.userEntry.info.id ,ClientCommands.writeField.id.toShort,ref,false,-1,{
  		TransactionManager.tryWriteInstanceField(ref,fieldNr,new IntConstant(newValue))
  	})
  }
  
  
	
  
  
	def handleDeutschAnswer(u:UserSocket,params:Seq[(String,Constant)]):Unit= {
		if(params.size==0)System.out.println("Stop")
		else{
			System.out.println("Anwsers:"+params)
			u.askEnquiry(aQuestion,handleDeutschAnswer )
		}
		
	}
  
}