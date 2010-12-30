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


case class TextPrintElement(nbounds:Rectangle2D.Float,text:String,fontStyle:String) extends PrintElement(nbounds) {
	//println("TXPE:"+text+" - "+fontStyle)
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
		g.drawString(text, x,y)
		val sbounds=fStyle.getStringBounds(text)	
		//println("sbounds:"+sbounds+" "+text)
		val newBonds=new Rectangle2D.Double((sbounds.getX+x),(sbounds.getY+y),sbounds.getWidth,sbounds.getHeight)
		//g.draw(newBonds)
		val baseLine=new Line2D.Double(x,y,sbounds.getWidth+x,y)
		//g.draw(baseLine)
		//g.drawRect((sbounds.getX+x).toInt,(sbounds.getY+y).toInt,sbounds.getWidth.toInt,sbounds.getHeight.toInt)
		//g.drawRect((x).toInt,(y).toInt,sbounds.getWidth.toInt,0)
	}
}

case class RectPrintElement(nbounds:Rectangle2D.Float,thick:Float) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
	}
	def getElementType= PrintElType.Rect 	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  PrintElement.printRect.setRect(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height))
	  thick match {
	  	case 0.4f => g.setColor(Color.blue)
	  	case 0.6f => g.setColor(Color.green)
	  	case _ =>
	  }
	  g.setStroke(PrintElement.strokeMap(thick))
		g.draw(PrintElement.printRect)		
		g.setColor(Color.black)
	}
}

object PrintElement {
	val printRect=new Rectangle2D.Float
	def apply(in:DataInput) = {		
		PrintElType(in.readByte) match {
			case PrintElType.TextField => TextPrintElement(readBounds(in),in.readUTF,in.readUTF)
			case PrintElType.Rect => RectPrintElement(readBounds(in),in.readFloat)
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