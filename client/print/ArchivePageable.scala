/**
 * Author: Peter Started:25.04.2011
 */
package client.print
import definition.data.InstanceData
import java.awt.print.PageFormat
import definition.data.PageData
import definition.data.RenderContext
import definition.data.FormDescription
import java.awt.print.Paper
import definition.data.FontStyleList
import java.io.ByteArrayInputStream
import definition.expression.BlobConstant
import java.io.DataInputStream
import definition.expression.DateConstant
import scala.swing.BoxPanel
import scala.swing.Label
import scala.swing.Orientation
import java.awt.Color
import scala.swing.Alignment
import javax.swing.BorderFactory
import scala.swing.FlowPanel
import scala.swing.GridPanel
import scala.swing.ListView
import java.util.Date

/**
 * 
 */
class ArchivePageable(data:InstanceData) extends APageable{
	def pageWidth:Float=data.fieldValue(2).toFloat
	def pageHeight:Float=data.fieldValue(3).toFloat
	
	var myContext:RenderContext=new RenderContext {
		def getScale:Double=1
	 var fontStyleList:FontStyleList=
		 FontStyleList.fromXML(scala.xml.XML.loadString( data.fieldValue(0).toString) )
	}
	override def context=if(tempContext==null)myContext else tempContext
	val leftBorder=data.fieldValue(5).toFloat
	val topBorder=data.fieldValue(6).toFloat
	val rightBorder=data.fieldValue(7).toFloat
	val bottomBorder=data.fieldValue(8).toFloat
	
	
	val paper=new Paper()
	paper.setSize(context.toUnit(pageWidth),context.toUnit(pageHeight))
  var pageFormat:PageFormat= new PageFormat	
  pageFormat.setPaper(paper)
  pageFormat.setOrientation(if(data.fieldValue(4).toBoolean) PageFormat.LANDSCAPE else PageFormat.PORTRAIT)
  println("Archive width:"+pageWidth+" height:"+pageHeight+" orient:"+pageFormat.getOrientation)
  
  
  var pagesList:Seq[PageData]= data.fieldValue(9) match {
		case b:BlobConstant => {
	    val inStream=new DataInputStream(new ByteArrayInputStream(b.data))
	    for( i <-0 until inStream.readInt) yield PageData(inStream)
		}
		case _ => Seq.empty	 
	}	
	  
	val date=data.fieldValue(10).toDate		
	
	override def toString= {
		DateConstant.shortDateTimeFormat.format(date) + " "+pagesList.size+" Seiten"
	}
}

class ArchiveRenderer() extends GridPanel(3,1 ) {
	val dateLabel=new Label
	val timeLabel=new Label
	val pageLabel=new Label
	//var dataIsSelected=false
	contents+=dateLabel+=timeLabel+=pageLabel	

	override def foreground_=(c: Color) = {
		super.foreground_=(c)
		dateLabel.foreground=c
		timeLabel.foreground=c
		pageLabel.foreground=c
	}
	
	override def background_=(c:Color) = {
		super.background_=(c)
		dateLabel.background=c
		timeLabel.background=c
		pageLabel.background=c
	}

	def config( isSelected: Boolean, focused: Boolean, a: ArchivePageable, index: Int) {
		//or whatever			
		opaque=true
		//firstLabel.opaque=true
		val now=DateConstant(new Date)
		val dist=now.dayDistance(DateConstant(a.date))
		dateLabel .text=if(dist==0) "Heute" else if(dist== -1) "Gestern" else  DateConstant.shortDateFormat.format(a.date)
		dateLabel.horizontalAlignment=Alignment.Center
		
		timeLabel .text=DateConstant.shortTimeFormat.format(a.date)+" Uhr"
		timeLabel.horizontalAlignment=Alignment.Center
		pageLabel .text=a.pagesList.size+" Seiten"
		pageLabel.horizontalAlignment=Alignment.Center
		

	} 
	border=BorderFactory.createCompoundBorder(
		BorderFactory.createLineBorder(Color.gray),
		BorderFactory.createEmptyBorder(5, 10, 5, 10))


}