/**
 * Author: Peter Started:29.12.2010
 */
package definition.data
import java.io.{DataInput,DataOutput}
import java.awt.{Font,Graphics,Graphics2D}
import java.awt.geom.{AffineTransform}
import java.awt.font.{FontRenderContext}
import definition.typ.SystemSettings
import java.awt.image.BufferedImage
import java.awt.Color

/**
 * 
 */
case class FontStyle(styleName:String,fontName:String,height:Float,bold:Boolean,italic:Boolean,underline:Boolean,color:Color) {
	
	lazy val font=new Font(fontName,(if(bold)Font.BOLD else 0) + (if(italic)Font.ITALIC else 0),(height*72.0/25.4).toInt)
	
	lazy val lineMetrics=font.getLineMetrics("QqpL", FontStyle.fontRenderCtx)
	
	lazy val mmHeight=lineMetrics .getHeight*25.4f/72.0f
	
	//var fontMetrics= getFontMetrics
	
	//private def getFontMetrics()= FontStyle.graphics.getFontMetrics(font)
	
	//def updateFontMetrics=fontMetrics=getFontMetrics
	
  def write(out:DataOutput)= {
  	out.writeUTF(styleName)
  	out.writeUTF(fontName)
  	out.writeFloat(height)
  	out.writeBoolean(bold)
  	out.writeBoolean(italic)
  	out.writeBoolean(underline)
  }
	
	def changeSize(scale:Float)=new FontStyle(styleName,fontName,height*scale,bold,italic,underline,color)
	
	def toXML= {
		<Font sname={styleName} fname={fontName} height={height.toString} bold={if(bold)"1" else "0"} italic={if(italic)"1" else "0"} 
		uline={if(underline)"1" else "0"} color={color.getRGB.toString}/>
	}
	
	def getStringBounds(st:String)={
		//println("Ctx:"+FontStyle.fontRenderCtx.getTransform+" "+FontStyle.fontRenderCtx.getTransformType)
		font.getStringBounds(st, FontStyle.fontRenderCtx)
	}
}

object FontStyle {	
	var fontRenderCtx:FontRenderContext=new FontRenderContext(null,true,true)
	val fontType=SystemSettings().systemTypes("StampFont")
		
	def apply(in:DataInput)= {
		new FontStyle(in.readUTF,in.readUTF,in.readFloat,in.readBoolean,in.readBoolean,in.readBoolean,new Color(in.readInt))
	}
	def fromXML(node:scala.xml.Node)= {
		new FontStyle((node\"@sname").text,(node\"@fname").text,(node\"@height").text.toFloat,(node\"@bold").text=="1",
			(node\"@italic").text=="1",(node\"@uline").text=="1",new Color((node\"@color").text.toInt))
	}
	
	def apply(data:InstanceData)= {
		new FontStyle(data.fieldValue(0).toString,data.fieldValue(1).toString,data.fieldValue(2).toDouble.toFloat,
			data.fieldValue(4).toBoolean,data.fieldValue(3).toBoolean,data.fieldValue(5).toBoolean,new Color(data.fieldValue(6).toInt))
	}
	
	//var graphics=new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB_PRE).getGraphics
	
	//def setGraphics(g:Graphics2D)=graphics=g
}


class FontStyleList(val list:Seq[FontStyle]) {
	private val fontMap=list.map(a=> (a.styleName.toLowerCase->a)).toMap
	
	lazy val standardStyle=list.find(_.styleName.equalsIgnoreCase("Standard")) match {
		case Some(style)=>println("Standard style found:"+style); style
		case None=>new FontStyle("Standard","Arial",10.f,false,false,false,Color.black)
	}
	
	def write(out:DataOutput) = {
		out.writeInt(list.size)
		list.foreach(_.write(out))
	}
	
	def toXML = {
		list.map(_.toXML)
	}
	
	def getStyle(stName:String)= if(fontMap.contains(stName.toLowerCase)) fontMap(stName.toLowerCase)
	else standardStyle
}

object FontStyleList {
	//var currentList:FontStyleList=null
	
	def apply(in:DataInput)= {
		new FontStyleList(for(i <-0 until in.readInt)yield FontStyle(in) )
	}
	
	def fromXML(node:scala.xml.Node)= {
		new FontStyleList((node\\"Font").map(FontStyle.fromXML))
	}
}