/**
 * Author: Peter Started:29.12.2010
 */
package client.print

import java.awt.print.{Pageable,PageFormat,Printable}
import java.awt.{Graphics,Graphics2D,Font,BasicStroke}
import definition.data.{PageData,RenderContext,FontStyleList}

/**
 * 
 */

object MyContext extends RenderContext {
	//def getFont(id:Int):Font=null
	def getScale:Double=1
	var fontStyleList:FontStyleList=new FontStyleList(Seq.empty)
}

class MyPageable extends Pageable with Printable {
	private var pageFormat:PageFormat= _	
	private var pagesList:Seq[PageData]=Seq.empty
	
	def setData(pf:PageFormat,pl:Seq[PageData])= {
		pageFormat=pf
		pagesList=pl
	}

  def getNumberOfPages(): Int = pagesList.size

  def getPageFormat(pageIndex: Int): PageFormat =  pageFormat

  def getPrintable(pageIndex: Int): Printable = this

  def print(g:Graphics,pf: PageFormat,pageIndex:Int) = {  	
  	if(pageIndex<pagesList.size) {
  		val g2=g.asInstanceOf[Graphics2D]
  		val ctx=g2.getFontRenderContext
  		//println("CTX:"+ctx )
  		//println("trans:"+ctx.getTransform+" "+ctx.getTransformType)
  		g2.setStroke(new BasicStroke(0.2f))
  		val page=pagesList(pageIndex)
  		for (el <-page.elementList ) {
  			el.print(g2, MyContext)
  		}  			
  		Printable.PAGE_EXISTS
  	} else Printable.NO_SUCH_PAGE
  }
}