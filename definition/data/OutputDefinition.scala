/**
 * Author: Peter Started:22.12.2010
 */
package definition.data
import definition.typ.SystemSettings
import definition.expression.{Constant,StringParser}


/**
 * 
 */
case class OutputDefinition(odInst:Int,formInst:Int,printer:String,paperSetting:String,portrait:Boolean,paramValues:Seq[(String,Constant)]) {
	var formName:String=""
		
	lazy val outName= getOutName
		
	def formPaper(s:String) = s.replace("iso-","").toUpperCase
	
	def getOutName= {
		val papers=paperSetting.split('|')
		//println("Papers "+papers.mkString("**")+ "\n"+ papers(0)+ " - " +papers(1))
		val paperText=if(papers.size>1) formPaper(papers(0))+" in "+
		(if(OutputDefinition.trayTranslations.contains(papers(1)))OutputDefinition.trayTranslations(papers(1)) else papers(1))
		else formPaper(paperSetting)		
		(if(formName.length==0) "Form "+formInst else formName)+" mit "+printer.split(" ").first+" auf Papier "+
			paperText+" | "+(if(portrait)" Hochformat" else " Querformat")
	}
	
		def toXML= <OutDef odInst={odInst.toString} formInst={formInst.toString} printer={printer} paper={paperSetting} portrait={if(portrait)"1" else "0"} >
    {paramValues.map(paramToXML(_))}
    </OutDef>	
  def paramToXML(p:(String,Constant))= <PValue name={p._1} value={p._2.getTerm}/>
  
  override def toString= outName
}

object OutputDefinition {
	val trayTranslations=Map(("bottom"->"Unten"),("top"->"Oben"),("manual"->"Manuell"),("Automatic-Feeder"->"Autom. Einzug"))
	val odefType=SystemSettings().systemTypes("OutputDef")
	
	def apply(data:InstanceData,paramChildren:Seq[InstanceData])= {
		if(data.ref .typ!=odefType) throw new IllegalArgumentException("Instance "+data+" is not an Outputdef")
		val paramList=paramChildren.map(c=> (c.fieldValue (0).toString,c.fieldValue(1)))
		new OutputDefinition(data.ref.instance,data.fieldValue(0).toInt,data.fieldValue(1).toString,data.fieldValue(2).toString,data.fieldValue(3).toBoolean,
			paramList)
	}	
	
	def paramFromXML(node:scala.xml.Node)= ((node\"@name").text,StringParser.parse((node\"@value").text).getValue )
	
	def fromXML(node:scala.xml.Node)= {
    new OutputDefinition((node\"@odInst").text.toInt,(node\"@formInst").text.toInt,(node\"@printer").text,(node\"@paper").text,
    	(node\"@portrait").text=="1",(node\\"PValue").map(paramFromXML(_)) )
	}
	
}