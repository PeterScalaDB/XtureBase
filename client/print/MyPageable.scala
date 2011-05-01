/**
 * Author: Peter Started:29.12.2010
 */
package client.print

import java.awt.print.{Pageable,PageFormat,Printable}
import java.awt.{Graphics,Graphics2D,Font,BasicStroke}
import definition.data.{PageData,RenderContext,FontStyleList}
import definition.data.FormDescription
import java.awt.geom.Rectangle2D

/**
 * 
 */

object MyContext extends RenderContext {
	//def getFont(id:Int):Font=null
	def getScale:Double=1
	var fontStyleList:FontStyleList=new FontStyleList(Seq.empty)
	val emptyRect=new Rectangle2D.Float(0,0,0,0)
}

abstract class APageable extends Pageable with Printable {
	def pageFormat:PageFormat	
	def pagesList:Seq[PageData]
	def context:RenderContext=if(tempContext==null)MyContext else tempContext	
	//def form:FormDescription
	def pageWidth:Float
	def pageHeight:Float
	def leftBorder:Float
	def topBorder:Float
	def rightBorder:Float
	def bottomBorder:Float
	
	var tempContext:RenderContext= _

  def getNumberOfPages(): Int = pagesList.size

  def getPageFormat(pageIndex: Int): PageFormat =  pageFormat

  def getPrintable(pageIndex: Int): Printable = this
  
  def clipRect=new Rectangle2D.Float(context.toUnit(leftBorder),context.toUnit( topBorder),context.toUnit(pageWidth-leftBorder-rightBorder), 
  		context.toUnit(pageHeight-topBorder-bottomBorder))

	
  def print(g:Graphics,pf: PageFormat,pageIndex:Int) = {  	
  	if(pageIndex<pagesList.size) {  		
  		val g2=g.asInstanceOf[Graphics2D]
  		val ctx=g2.getFontRenderContext
  		//println("CTX:"+ctx )
  		//println("trans:"+ctx.getTransform+" "+ctx.getTransformType)
  		val cb=g.getClipBounds
  		val newClip=if(cb!=null)cb.createIntersection(clipRect) else clipRect
  		//g.setClip(newClip)
  		g2.setStroke(new BasicStroke(0.2f))
  		val page=pagesList(pageIndex)
  		for (el <-page.elementList ) {
  			el.print(g2, context)
  		}  			
  		Printable.PAGE_EXISTS
  	} else Printable.NO_SUCH_PAGE
  }
}


class MyPageable extends APageable {
	var pageFormat:PageFormat= _	
	var pagesList:Seq[PageData]=Seq.empty
	//var context:RenderContext=MyContext	
	var form:FormDescription=_
	def leftBorder:Float= if(form==null)0f else form.left
	def topBorder:Float= if(form==null)0f else form.top
	def rightBorder:Float= if(form==null)0f else form.right
	def bottomBorder:Float= if(form==null)0f else form.bottom
	def pageWidth:Float=if(form==null)0f else form.toMM(pageFormat .getWidth)
	def pageHeight:Float=if(form==null)0f else form.toMM(pageFormat .getHeight)	
	
	def setData(pf:PageFormat,pl:Seq[PageData])= {
		pageFormat=pf
		pagesList=pl
	}
  
}