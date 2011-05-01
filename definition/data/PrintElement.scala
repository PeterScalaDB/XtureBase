/**
 * Author: Peter Started:27.12.2010
 */
package definition.data

import java.io.{DataInput,DataOutput}
import java.awt.{Rectangle,Font,Graphics2D,BasicStroke,Color}
import java.awt.geom.{Rectangle2D,Line2D}

/**
 * 
 */
abstract class PrintElement(val bounds:Rectangle2D.Float) {
	
	def write(out:DataOutput)= {
		out.writeByte(getElementType.id.toByte)
		out.writeFloat(bounds.x)
		out.writeFloat(bounds.y)
		out.writeFloat(bounds.width)
		out.writeFloat(bounds.height)
	}	
	def getElementType:PrintElType.Value
	
	def print(g:Graphics2D,ctx:RenderContext)
}

object PrintElType extends Enumeration {
	val TextField=Value("TextElement")
	val Line=Value("LineElement")
	val TextArea=Value("TextArea")
	val Rect=Value("RectElement")
}

trait RenderContext {
	//def getFont(id:Int):Font
	def getScale:Double
	def toUnit(mm:Double):Float = (mm*72.0/25.4).toFloat
	def fontStyleList:FontStyleList
}


abstract class ATextPrintElement(nbounds:Rectangle2D.Float,fontStyle:String) extends PrintElement(nbounds) {
	//println("TXPE:"+text+" - "+fontStyle)
	
	def text:String
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeUTF(text)
		out.writeUTF(fontStyle)		
	}
	def getElementType= PrintElType.TextField	
	
	def print(g:Graphics2D,ctx:RenderContext) = {		
		val fStyle=ctx.fontStyleList.getStyle(fontStyle)		
		val offset=fStyle.lineMetrics.getAscent
		val x=ctx.toUnit(bounds.x)
		val y=ctx.toUnit(bounds.y)+offset		
		g.setFont(fStyle.font)
		g.setColor(fStyle.color)
		g.drawString(text, x,y)
		//val sbounds=fStyle.getStringBounds(text)		
		//val newBonds=new Rectangle2D.Double((sbounds.getX+x),(sbounds.getY+y),sbounds.getWidth,sbounds.getHeight)				
	}
}


class TextPrintElement(nbounds:Rectangle2D.Float,val text:String,fontStyle:String) extends ATextPrintElement(nbounds,fontStyle) {	
}


class PlaceHolderElement(nbounds:Rectangle2D.Float,val name:String,fontStyle:String) extends ATextPrintElement(nbounds,fontStyle) {
	println("new PlaceHolder:"+name)
	
	var value:String=""
	def text=value
}


case class RectPrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,borderColor:Color) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(borderColor.getRGB)
	}
	def getElementType= PrintElType.Rect 	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  PrintElement.printRect.setRect(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height))
	  g.setColor(borderColor)
	  g.setStroke(PrintElement.strokeMap(thick))
		g.draw(PrintElement.printRect)		
	}
}

case class LinePrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,lineColor:Color) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(lineColor.getRGB)
	}
	def getElementType= PrintElType.Line 	
	def print(g:Graphics2D,ctx:RenderContext) = if(thick!=0){
	  PrintElement.printLine.setLine(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.x+bounds.width),ctx.toUnit(bounds.y+bounds.height))
	  g.setColor(lineColor)
	  g.setStroke(PrintElement.strokeMap(thick))	  
		g.draw(PrintElement.printLine)		
	}
}

object PrintElement {
	val printRect=new Rectangle2D.Float
	val printLine=new Line2D.Float
	def apply(in:DataInput) = {		
		PrintElType(in.readByte) match {
			case PrintElType.TextField => new TextPrintElement(readBounds(in),in.readUTF,in.readUTF)
			case PrintElType.Rect => RectPrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt))
			case PrintElType.Line => LinePrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt))
			case other => throw new IllegalArgumentException("Unknown PrintElement-Type:"+other)
		}
		
	}
	
	def readBounds(in:DataInput)= new Rectangle2D.Float(in.readFloat,in.readFloat,in.readFloat,in.readFloat)
	
	val strokeMap=new FactoryMap[Float,BasicStroke](d=> {
		if (d==0.4f) new BasicStroke(0.3f, BasicStroke.CAP_SQUARE ,BasicStroke.JOIN_BEVEL , 1f, Array(3f,2f),0f) 
		else if (d==0.6f) new BasicStroke(0.3f, BasicStroke.CAP_SQUARE ,BasicStroke.JOIN_BEVEL , 1f, Array(1f,5f),0f) 
		else new BasicStroke(d)
		})	
}


class FactoryMap[A,B](factory:(A)=>B) extends collection.mutable.HashMap[A,B] {
	override def apply(a:A)= if (contains(a)) super.apply(a)
	else {
		val n=factory(a)
		update(a,n)
		n
	}
}