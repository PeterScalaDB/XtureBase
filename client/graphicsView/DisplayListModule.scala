/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import server.storage.{ActionModule,ActionImpl}
import definition.typ._
import definition.expression._
import definition.data._
import java.io._
import transaction.handling.TransactionManager

import scala.collection.Iterator

/**
 * 
 */
class DisplayListModule extends ActionModule {

	
	
  def getActionsIterator() =  mList.iterator 
  
  val importAction=new ActionImpl("DXF-Import",Some(new ParamQuestion("Welche DXF Datei importieren ?",
		Seq(new ParamAnswerDefinition("Dateinamen Eingeben:",DataType.StringTyp,None)))),doImport)
  
  val mList=List(importAction)
  
  def doImport(data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
  	if(!param.isEmpty) {
  		val startTime=System.currentTimeMillis();
  		val dateiName=param(0)._2.toString
  		val file=new File(dateiName)  		
  		val reader=new BufferedReader(new FileReader(file))
  		val owner=Array(new OwnerReference(0,data.ref))
  		
  		var line=reader.readLine
  		while(line!=null) {  			
  			if(line.trim=="AcDbLine")
  			{
  				var x1,y1,x2,y2=0d
  				line=reader.readLine
  				while(line!=" 31") {
  					if(line(0)==' ') {
  						val field=line.drop(1).toInt
  						line=reader.readLine  						
  						val coord=line.trim.toDouble
  						field match{
  							case 10 =>x1=coord
  							case 20 =>y1=coord
  							case 11 =>x2=coord
  							case 21 =>{
  								y2=coord
  								//println("Line ("+x1+","+y1+")-("+x2+","+y2+")")
  								var inst=TransactionManager.tryCreateInstance(40,owner,false)
  								inst=inst.setField(3,new VectorConstant(x1,y1,0))
  								inst=inst.setField(4,new VectorConstant(x2,y2,0))
  								// write ignoring collfuncs and refs
  								TransactionManager.tryWriteInstanceData(inst)
  							} 
  							case _ =>
  						}
  					}
  					line=reader.readLine
  				}
  			}
  			line=reader.readLine
  		}
  		reader.close
  		val endTime=System.currentTimeMillis()
  		println("Import ready "+dateiName+" time:"+(endTime-startTime))
  	}
  	true
  }
}