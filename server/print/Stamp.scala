/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import java.io.{DataInput,DataOutput}
import definition.data._
import server.storage.StorageManager
import transaction.handling.SessionManager
import definition.typ.{SystemSettings,DataType}
import definition.expression.{StringConstant,Expression}
import java.awt.geom.Rectangle2D
import java.awt.font.{LineBreakMeasurer}
import java.text.AttributedString
import definition.typ.HorAlign
import java.awt.Color

/**
 * 
 */
/*object SizeType extends Enumeration {
	val Fixed=Value("Fixed")
	val MaxOfMin=Value("MaxOfMin")
	val MinOfMax=Value("MinOfMax")
	val SumOfMax=Value("SumOfMax")
	val SumOfMin=Value("SumOfMin")
}*/



trait StampElement {
	def minWidth:Float
	def maxWidth:Float
	def minHeight:Float
	def maxHeight:Float
	
	def updateVariables(cx:Context)
	def getPrintElements(x:Float,y:Float,restWidth:Float,restHeight:Float):Seq[PrintElement]
	def setRenderWidth(nw:Float)
	def setRenderHeight(nh:Float)= {}
}

object StampElement {
	val textGap=1
	val springSize=10
	var stampTextType:Int= -1
	var stampBoxType:Int= -1
	var stampValueSetterType:Int = -1
	var stampType:Int= -1
	var stampFontType = -1
	var stampLineType:Int= -1
	
	var constructorMap=new collection.mutable.HashMap[Int,(FormDescription,InstanceData)=>StampElement]()
	
	//println("create stampElement")
	SessionManager.registerSetupListener(()=>{
		stampTextType=SystemSettings().systemTypes("StampText")
		stampBoxType=SystemSettings().systemTypes("StampBox")
		stampType=SystemSettings().systemTypes("Stamp")
		stampFontType=SystemSettings().systemTypes("StampFont")
		stampValueSetterType = SystemSettings().systemTypes("StampValueSetter")
		stampLineType= SystemSettings().systemTypes("StampLine")
		
		constructorMap(stampTextType)=StampText(_:FormDescription,_:InstanceData)
		constructorMap(stampBoxType)=new StampBox(_:FormDescription,_:InstanceData)
		constructorMap(stampType)=new Stamp(_:FormDescription,_:InstanceData)
		constructorMap(stampValueSetterType)= new StampValueSetter(_:FormDescription,_:InstanceData)
		constructorMap(stampLineType)=new StampLine(_:FormDescription,_:InstanceData)
		//println("ConstructorMap :"+constructorMap.mkString("\n"))
	})
	
	def apply(form:FormDescription,data:InstanceData) = {
		constructorMap(data.ref.typ)(form,convertVariables(data))
	}
	
	def readChildren(nForm:FormDescription,parent:Reference,propField:Byte)= {
		StorageManager.getInstanceProperties(parent) match {
			case Some(pData) => pData.propertyFields (propField).propertyList .map(a =>StampElement(nForm,StorageManager.getInstanceData( a )))
			case None => Seq.empty
		}
	}
	
	def convertVariables(data:InstanceData)= {		
		new InstanceData(data.ref, data.fieldData .map(a=> a.replaceExpression(_ match {
			case s:StringConstant => {
				var name=s.n
				if(isPrintVar(s.n)) new PrintVariable(s.n .substring(3))
				else if(isPlaceHolderPrintVar(s.n)) new PlaceHolderPrintVariable(s.n.substring(4,s.n.size-1))
				else s
			} 
			case other => other
		})),data.owners,data.secondUseOwners,data.hasChildren)
	}
	
	def isPrintVar(st:String)= if(st.length>3){
		(st.charAt(0)=='p'||st.charAt(0)=='P')&&
    (st.charAt(1)=='r'||st.charAt(1)=='R')&&
    st.charAt(2)=='_'
	} else false
	
	def isPlaceHolderPrintVar(st:String)= if(st.length>5){
		(st.charAt(0)=='#')&&(st.charAt(st.size-1)=='#')&&
		(st.charAt(1)=='p'||st.charAt(1)=='P')&&
    (st.charAt(2)=='r'||st.charAt(21)=='R')&&
    st.charAt(3)=='_'
	} else false
}


class StampText (form:FormDescription,val text:Expression,val mWidth:Float,val maxWidth:Float,val mHeight:Float,val maxHeight:Float,val wordWrap:Boolean,
	val horAlign:HorAlign.Value,val fontStyle:FontStyle)	
	 extends StampElement {	
	def this(form:FormDescription,data:InstanceData) = {
		this(form,data.fieldData(0),data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
			data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),
			form.fonts.getStyle( data.fieldData(7).toString))
	}
	
	var renderWidth=mHeight
	
	override def toString="StampText '"+text+"' minW:"+minWidth+" maxW:"+maxWidth+" minH:"+minHeight+" maxH:"+maxHeight+" hAlign:"+horAlign+
	" wrap:"+wordWrap+" font:"+fontStyle
	
	def updateVariables(cx:Context)= {
		text.foreach(_ match {
			case p:PrintVariable=> p.updateVariable(cx)//;println("update "+p+"->"+p.value)
			case o => 
		})		
	}
	
	def getPrintElements(x:Float,y:Float,restWidth:Float,restHeight:Float)= {
				
		if(wordWrap==false) {			
		  val tx=text.getValue.toString
			val sbounds=fontStyle.getStringBounds(tx)
			val offset:Float=horAlign match {
				case HorAlign.Left|HorAlign.Block => 0.f
				case HorAlign.Center =>(restWidth-form.toMM(sbounds.getWidth))/2f
				case HorAlign.Right => restWidth-form.toMM(sbounds.getWidth)
			}
			//println("restWidth:"+restWidth+" tomm:"+form.toMM(sbounds.getWidth)+" offset:"+offset)
			List(createTextElement(new Rectangle2D.Float(x+offset,y,restWidth,restHeight),tx,fontStyle.styleName)/*,
				new RectPrintElement(new Rectangle2D.Float(x,y,restWidth,restHeight),0.2f)*/)
		} else { // wordwrap
			var lh=0f
			var retList:List[PrintElement]=Nil
			val rb=form.fromMM(restWidth)
			for(tx <-text.getValue.toString.split('\n')) {
				val as=new AttributedString(if(tx.length==0)" " else tx,fontStyle.font .getAttributes)
				val measurer=new LineBreakMeasurer(as.getIterator,FontStyle.fontRenderCtx)			
				var lastPos=0			
				while (measurer.getPosition < tx.size) {
					val lay=measurer.nextLayout(rb)
					val subText=tx.substring(lastPos,measurer.getPosition)				
					val ah=lay.getAscent+lay.getDescent
					val aw=form.toMM(lay.getAdvance+(if(subText.size>0 && subText.last==' ')-fontStyle.getStringBounds(" ").getWidth else 0f))
					val offset:Float=horAlign match {
						case HorAlign.Left|HorAlign.Block => 0.f
						case HorAlign.Center =>(restWidth-aw)/2f
						case HorAlign.Right => restWidth-aw
					}
					retList=new TextPrintElement(new Rectangle2D.Float(x+offset,y+lh,restWidth,form.toMM(ah)),subText,fontStyle.styleName) :: retList
					lh+=form.toMM(lay.getLeading+ah)
					lastPos=measurer.getPosition
				}		
				//retList=new RectPrintElement(new Rectangle2D.Float(x,y,restWidth,restHeight),0.2f):: retList	
			}
			retList
		}
	}
	
	def createTextElement(rect:Rectangle2D.Float,ntext:String,nfont:String):ATextPrintElement= new TextPrintElement(rect,ntext,nfont)
		
	def minWidth=if(mWidth== 0) {		
			val sbounds=fontStyle.getStringBounds(text.getValue.toString)
			form.toMM(sbounds.getWidth)+StampElement.textGap
		} 
	 else mWidth //fixed
	
	
	def minHeight=if(mHeight== 0) {
		if(wordWrap==false) {
			val sbounds=fontStyle.getStringBounds(text.getValue.toString)
			form.toMM(sbounds.getHeight)+StampElement.textGap
		} 
		else { // wordwrap
			var lh=0f
			//println("Word wrap "+text.getValue.toString+" "+renderWidth)
			if(renderWidth>0)for(tx <-text.getValue.toString.split('\n')) {
				val as=new AttributedString(if(tx.length==0)" " else tx,fontStyle.font .getAttributes)
				val rb=form.fromMM(renderWidth)
				val measurer=new LineBreakMeasurer(as.getIterator,FontStyle.fontRenderCtx)

				while (measurer.getPosition() < tx.size) {
					val lay=measurer.nextLayout(rb)
					lh+=lay.getAscent+lay.getDescent+lay.getLeading
				}	
			}
			//println("Word wrap "+text.getValue.toString+" "+renderWidth+" ->"+form.toMM(lh))		
			form.toMM(lh)
		}
	} else mHeight // fixed
	
	
	override def setRenderWidth(nw:Float)= {renderWidth=nw}
}

class StampPlaceHolder (nform:FormDescription,ntext:Expression,miWidth:Float,maWidth:Float,miHeight:Float,maHeight:Float,wWrap:Boolean,
	hAlign:HorAlign.Value,fStyle:FontStyle)	
	 extends StampText (nform,ntext,miWidth,maWidth,miHeight,maHeight,wWrap,hAlign,fStyle) {	 
	
		def this(form:FormDescription,data:InstanceData) = {
			this(form,data.fieldData(0),data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
				data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),
				form.fonts.getStyle( data.fieldData(7).toString))
		}
		
		override def createTextElement(rect:Rectangle2D.Float,ntext:String,nfont:String)={
			val newEl=new PlaceHolderElement(rect,ntext,nfont)
			PrintGenerator.context.createPlaceHolder(ntext,newEl)
			newEl
		}
	}

object StampText {
	private def withCross(text:String)=text.size>2&&text.charAt(0)=='#'&&text.charAt(text.size-1)=='#'
		
	def apply(form:FormDescription,data:InstanceData)={
		data.fieldData(0) match {			
			case p:PlaceHolderPrintVariable=> {       	
        	new StampPlaceHolder (form,p,data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
        		data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),form.fonts.getStyle( data.fieldData(7).toString))				
			}			
			case s:StringConstant=> {
        val text=s.n
        if(withCross(text)) {
        	val newExpression=new StringConstant(text.substring(1,text.size-1))
        	new StampPlaceHolder (form,newExpression,data.fieldValue(1).toFloat,data.fieldValue(2).toFloat,data.fieldValue(3).toFloat,
        		data.fieldValue(4).toFloat,data.fieldValue(5).toBoolean,HorAlign(data.fieldValue(6).toInt),form.fonts.getStyle( data.fieldData(7).toString))
        }
        else new StampText(form,data)
			}
			case _=> new StampText(form,data)
		}
	}
}

class StampValueSetter(form:FormDescription,name:Expression,value:Expression) extends StampElement {
	def this(form:FormDescription,data:InstanceData) = this(form,data.fieldData(0),data.fieldData(1))
	//println("setter created "+name+" :" +value.toString+"="+value.getTerm)
	def minWidth:Float=0.01f
	def maxWidth:Float=0f
	def minHeight:Float=0.01f
	def maxHeight:Float=0f	

	def updateVariables(cx:Context)= {
		def check(ex:Expression) = ex match {
			case p:PrintVariable=> p.updateVariable(cx)//;println("setter update "+p+"->"+p.value)
			case o => 
		}
		name.foreach(check)			
		value.foreach(check)		
	}	
	
	def getPrintElements(x:Float,y:Float,restWidth:Float,restHeight:Float):Seq[PrintElement] = {
		PrintGenerator.context.setPlaceHolderValue(name.getValue.toString,value.getValue.toString)
		Seq.empty
	}
	
	def setRenderWidth(nw:Float)= {}
}

class StampLine(form:FormDescription,val minWidth:Float,val minHeight:Float,isRectangle:Boolean,val lineThickness:Float,
	val lineStyle:Byte,val color:Color) extends StampElement {
	def this(form:FormDescription,data:InstanceData) = {
		this(form,data.fieldValue(0).toFloat,data.fieldValue(1).toFloat,data.fieldValue(2).toBoolean,data.fieldValue(3).toFloat,
			data.fieldValue(4).toInt.toByte,new Color(data.fieldValue(5).toInt))
	}
	var renderWidth:Float = _
	var renderHeight:Float = _
	def maxWidth=minWidth
	def maxHeight=minHeight
	def updateVariables(cx:Context)= {}
	
	def getPrintElements(x:Float,y:Float,restWidth:Float,restHeight:Float):Seq[PrintElement]= {
		val rect=new Rectangle2D.Float(x,y,if(minWidth<0)renderWidth else minWidth,
			if(minHeight<0)renderHeight else minHeight)
		if (isRectangle) List(new RectPrintElement(rect,lineThickness,lineStyle,color))
		else List(new LinePrintElement(rect,lineThickness,lineStyle,color))
	}
	
	def setRenderWidth(nw:Float) = {renderWidth=nw}
	override def setRenderHeight(nh:Float) = {renderHeight=nh}
}



class StampBox(form:FormDescription,val horOrient:Boolean,val width:Float,val height:Float,
	val gap:Double, val frontChildren:Seq[StampElement],val centerChildren:Seq[StampElement],
	val bottomChildren:Seq[StampElement]) extends StampElement {
	
	def this(form:FormDescription,data:InstanceData)= {
		this(form,data.fieldValue(0).toBoolean,data.fieldValue(1).toFloat,
			data.fieldValue(2).toFloat,data.fieldValue(3).toDouble,StampElement.readChildren(form,data.ref,0),StampElement.readChildren(form,data.ref,1),
			StampElement.readChildren(form,data.ref,2))
			inst=data.ref
	}
	var inst:Reference=_
	
	def minWidth:Float ={		
		val r=if(width==0) {
			var retWidth=0f
			if (horOrient) foreach(el=> {
				val w=el.minWidth;
				if(w== -1) return -1
				else retWidth+= w
			})
			else foreach(el => {
				val w=el.minWidth
				if(w== -1)return -1
				else if(w>retWidth) retWidth=w
			}) 
			retWidth			
		} else width
		//println("Box MinWidth:"+inst.sToString+" Hor:"+horOrient+" value:"+r)
		r
	}
	def maxWidth=width
	
	def minHeight:Float= {
			val r=if(height==0) {			

				if (horOrient){
					var retHeight= -1f
					foreach(el => {					
						val h=el.minHeight				
						if(h>retHeight) retHeight=h				
					})
					retHeight
				}
				else{ 
					var retHeight=0f
					foreach(el=> {					
						val h=el.minHeight;
						if(h== -1) return -1
						else retHeight+= h					
					})
					retHeight
				}
			} else height
			//println("Box MinHeight:"+inst.sToString+" Hor:"+horOrient+" value:"+r)
			r
		}	
	
		
	def maxHeight=height
	
	def updateVariables(cx:Context)= {
		def sub(se:StampElement)= se.updateVariables(cx)		
		frontChildren.foreach(sub)
		centerChildren.foreach(sub)
		bottomChildren.foreach(sub)
	}
	
	def foreach(f:(StampElement)=>Unit)= { 
		frontChildren.foreach(f)
		centerChildren.foreach(f)
		bottomChildren.foreach(f)
	}
	
	def flatMap[A](f:(StampElement)=>Traversable[A])= {
		frontChildren.flatMap(f)++ centerChildren.flatMap(f)++ bottomChildren.flatMap(f)
	}
	
	def getPrintElements(x:Float,y:Float,restWidth:Float,restHeight:Float)= {
		var currX=x
		var currY=y
		var numSpring=0f
		var sumFixed=0f
		var scale=1f
		var springSize=0f
		// gather spring information
		if(horOrient){
			foreach(el =>{
				if(el.minWidth== -1) numSpring += 1
				else sumFixed+=el.minWidth
			})
			if(sumFixed+numSpring*StampElement.springSize > restWidth)
				scale= 	restWidth/(sumFixed+numSpring*StampElement.springSize)			
			if(numSpring>0) springSize=(restWidth-scale*sumFixed)/numSpring
			foreach(el => {
				val elWidth=el.minWidth
				el.setRenderWidth(if(elWidth== -1)springSize else elWidth)
			})
			val th=minHeight
			val theHeight=if(th== -1)restHeight else th
			if(inst==Reference(75,4))println("GETPE"+inst.sToString+ " hor: numSpring:"+numSpring+" sumFixed:"+sumFixed+" springS:"+springSize+" minHeight:"+minHeight+" restw:"+restWidth+" resth:"+restHeight)
			foreach(el => {			
				if(el.minHeight== -1) el.setRenderHeight(theHeight)
			})
			flatMap(el => {
				val elWidth=el.minWidth				
				val elHeight=el.minHeight
				val theWidth=if(elWidth== -1)springSize else elWidth
				if(inst==Reference(75,4)) println("Set h element:"+el.getClass+" cX:"+currX+" cy:"+currY+" elWidth:"+elWidth+" theWidth:"+theWidth+" elHeight:"+elHeight)
			  val ret=el.getPrintElements(currX, currY,theWidth , if(elHeight== -1) theHeight else elHeight)
			  currX+= theWidth
			  ret
			}) /*++List( new RectPrintElement(new Rectangle2D.Float(x,y,minWidth,minHeight),0.4f) ,
			new TextPrintElement(new Rectangle2D.Float(x,y+minHeight,10,10),"H"+inst.sToString,"Mini"))*/
		}
		else { // vertical
			foreach(el =>{
				if(el.minHeight== -1) numSpring += 1
			else sumFixed+=el.minHeight
			})		
			if(sumFixed+numSpring*StampElement.springSize > restHeight){
				scale= 	restHeight/(sumFixed+numSpring*StampElement.springSize)
				println("scale= "+scale+ " "+inst)
			}
			if(numSpring>0) springSize=(restHeight-scale*sumFixed)/numSpring
			foreach(el => {
				if(el.minHeight== -1)				
				  el.setRenderHeight(springSize)
			})
			val tw=minWidth
			val theWidth=if(tw== -1)restWidth else tw
			foreach(el => {
				val elWidth=el.minWidth
				el.setRenderWidth(if(elWidth== -1)theWidth else elWidth)
			})
			if(inst==Reference(74,6)) println("GETPE v: "+inst.sToString+" numSpring:"+numSpring+" sumFix:"+sumFixed+" springS:"+springSize+" minWidth:"+minWidth+" restw:"+restWidth+" minH:"+minHeight+" resth:"+restHeight)
			flatMap(el => {
				val elHeight=el.minHeight				
				val elWidth=el.minWidth
				val theHeight=if(elHeight== -1)springSize else elHeight
				if(inst==Reference(74,6)||(inst==Reference(75,4))) println("Set v :"+el.getClass+" cX:"+currX+" cy:"+currY+" elWidth:"+elWidth+" theWidth:"+theWidth+" elHeight:"+elHeight+" theHeight"+theHeight)
			  val ret=el.getPrintElements(currX, currY,if(elWidth== -1) theWidth else elWidth,theHeight)
			  currY+= theHeight
			  ret
			}) /*++List( new RectPrintElement(new Rectangle2D.Float(x,y,tw,minHeight),0.6f) ,
			new TextPrintElement(new Rectangle2D.Float(x+6,y+minHeight,10,10),"V"+inst.sToString,"Mini"))*/
		}		
	}
	def setRenderWidth(nw:Float)= {
		if(!horOrient){
			foreach(el =>{
				if(el.minWidth== -1)el.setRenderWidth(nw)
			})
		}
	}
  
}


case class Stamp(form:FormDescription,val forType:Int,val name:String,nhorOrient:Boolean,/*nwidthSetting:SizeType.Value,*/nwidth:Float,/*nheightSetting:SizeType.Value,*/nheight:Float,
	ngap:Double,nfrontChildren:Seq[StampElement],ncenterChildren:Seq[StampElement],nbottomChildren:Seq[StampElement]) 
	extends StampBox(form,nhorOrient,/*nwidthSetting,*/nwidth,/*nheightSetting,*/nheight,ngap,nfrontChildren,ncenterChildren,nbottomChildren) {
  
	def this(form:FormDescription,data:InstanceData )= {
		this(form,data.fieldValue(4).toInt,data.fieldValue(5).toString,data.fieldValue(0).toBoolean,/*SizeType(data.fieldValue(1).toInt),*/data.fieldValue(1).toFloat,/*SizeType(data.fieldValue(3).toInt),*/
			data.fieldValue(2).toFloat,data.fieldValue(3).toDouble,StampElement.readChildren(form,data.ref,0),StampElement.readChildren(form,data.ref,1),
			StampElement.readChildren(form,data.ref,2))
			inst=data.ref
	}
	
	override def toString= "Stamp "+name+" for Type:"+forType+" horOrient:"+horOrient/*+" wSet:"+widthSetting*/+" w:"+width/*+" hSet:"+heightSetting*/+
	" h:"+height+"\nFrontChildren:"+frontChildren.mkString("\n")+"\nCenterChildren:"+centerChildren.mkString("\n")+
	"\nBottomChildren:"+bottomChildren.mkString("\n")
}




