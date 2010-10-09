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

			var code="0"
			var value=""
			var section=""
			while (section!="ENTITIES") {
				while(code.trim.toInt!=0 &&value!="SECTION"){
					code=reader.readLine
					value=reader.readLine
				}
				code=reader.readLine
				section=reader.readLine
			}

			var elemTyp=""
				code=""
					while(elemTyp!="SECTION") {
						while (code!="  0") {
							code=reader.readLine
							//println("Code1:"+code)
						}
						elemTyp=reader.readLine
						//println("elTyp:"+elemTyp)
						elemTyp match {
							case "LINE" => {
								  code==""
									while(code!="370") code=reader.readLine
									val thickness=reader.readLine.trim.toInt
									var x1,y1,x2,y2=0d
									var color:Int=0
									code=reader.readLine
									while(code!=" 31") {
										//println("code "+code)
										if(code(0)==' ') {
											val field=code.trim.toInt  			    	  						
											val value=reader.readLine
											field match{
												case 6 => // linestyle
												case 10 =>x1=value.trim.toDouble
												case 20 =>y1=value.trim.toDouble
												case 11 =>x2=value.trim.toDouble
												case 21 =>{
													y2=value.trim.toDouble
													println("Line ("+x1+","+y1+")-("+x2+","+y2+")")
													var inst=TransactionManager.tryCreateInstance(40,owner,false)
													inst=inst.setField(3,new VectorConstant(x1,y1,0))
													inst=inst.setField(4,new VectorConstant(x2,y2,0))
													inst=inst.setField(0,new IntConstant(AcadColor.table(color)))
													inst=inst.setField(1,new IntConstant(thickness))
													// write ignoring collfuncs and refs
													TransactionManager.tryWriteInstanceData(inst)
													color=0
												} 
												case 62 => color=value.trim.toInt  			    		
												case _ =>  			    	
											}
											//code=reader.readLine
										}
										code=reader.readLine
									}  			     		
							}
							case "ARC" => {
									code==""
									while(code!="370") code=reader.readLine
									val thickness=reader.readLine.trim.toInt
									var x1,y1,dia,sA,eA=0d
									var color:Int=0
									
									do {
										code=reader.readLine
										//println("code "+code)
										if(code(0)==' ') {
											val field=code.trim.toInt  			    	  						
											val value=reader.readLine
											field match{
												case 6 => // linestyle
												case 10 =>x1=value.trim.toDouble
												case 20 =>y1=value.trim.toDouble
												case 40 =>dia=value.trim.toDouble
												case 50 =>sA=value.trim.toDouble												
												case 51 =>{
													eA=value.trim.toDouble
													println("Arc ("+x1+","+y1+") d="+dia+" ,sa:"+sA+" ,ea:"+eA+")")
													var inst=TransactionManager.tryCreateInstance(41,owner,false)
													inst=inst.setField(3,new VectorConstant(x1,y1,0))
													inst=inst.setField(4,new DoubleConstant(dia))
													inst=inst.setField(5,new DoubleConstant(sA))
													inst=inst.setField(6,new DoubleConstant(eA))
													inst=inst.setField(0,new IntConstant(AcadColor.table(color)))
													inst=inst.setField(1,new IntConstant(thickness))
													// write ignoring collfuncs and refs
													TransactionManager.tryWriteInstanceData(inst)
													color=0
												} 
												case 62 => color=value.trim.toInt  			    		
												case _ =>  			    	
											}
											//code=reader.readLine
										}
										
									} while(code!=" 51") 		
							}
							case _ =>
						}

					}
			reader.close
			val endTime=System.currentTimeMillis()
			println("Import ready "+dateiName+" time:"+(endTime-startTime))
		}
		true
	}
}