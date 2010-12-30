/**
 * Author: Peter Started:22.12.2010
 */
package client.print

import scala.swing._
import scala.swing.event._
import java.awt.event.{WindowAdapter,WindowEvent}
import javax.swing.{BorderFactory,DefaultComboBoxModel}
import javax.swing.border.TitledBorder
import client.dataviewer.{FieldColumnModel,MultilineEditor}
import definition.expression.{Expression,Constant,EMPTY_EX}
import definition.data.{FormDescription,Reference,OutputDefinition}

import javax.print._
import javax.print.attribute._
import javax.print.attribute.standard._
import javax.print.event._
import sun.print.ServiceDialog
import java.awt.print._
import java.awt.{Graphics,Graphics2D}

/**
 * 
 */
class NewOutdefDialog (w:Window) extends Dialog(w) {	
	
	private var outputDefinedFunc:(Int,String,String,Boolean,Int,Int,Seq[(String,Constant)])=>Unit = _
	
  val emptyAttributeSet=new HashPrintRequestAttributeSet();
	var pras:PrintRequestAttributeSet  = new HashPrintRequestAttributeSet();
	val printJob=PrinterJob.getPrinterJob
	val printServices=  PrintServiceLookup.lookupPrintServices( null,null);
	if(printServices==null || printServices.size==0) System.err.println("no print services found !")
	var printerName:String=null
	var theService:javax.print.PrintService= PrintServiceLookup.lookupDefaultPrintService
	var lastSelectedMedia:MediaSizeWrapper=MediaMap.getMediaSize(theService.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName])
	setMediaWrapper(lastSelectedMedia)
	var combosAdjusting=false
	
	val mediaModel=new DefaultComboBoxModel
	val mediaCombo:ComboBox[MediaSizeWrapper]=new ComboBox(List(new MediaSizeWrapper(MediaSizeName.ISO_A4))){
		peer.setModel(mediaModel)
		listenTo(this.selection)
		reactions+= {
			case SelectionChanged(e) if(!combosAdjusting) => setMediaWrapper(mediaCombo.selection.item)		
		}
	}
	val trayModel=new DefaultComboBoxModel
	val trayCombo:ComboBox[MediaTrayWrapper]=new ComboBox(List(new MediaTrayWrapper(MediaTray.MAIN))){
		peer.setModel(trayModel)
		listenTo(this.selection)
		reactions+= {
			case SelectionChanged(e) if(!combosAdjusting) => {
				val mediaTray=trayCombo.selection.item
				//val mediaSize=javax.print.attribute.standard.MediaSize.getMediaSizeForName(mediaName)
				println("select mediaTray:"+mediaTray+" "+mediaTray.getClass)	
				if(mediaTray!=null){
				  if(mediaTray==AutoTray) {
				  	pras.remove(classOf[MediaTray])
				  	pras.remove(TrayMap.altClass)
				  }
				  else pras.add(mediaTray.altValue)
				}				
			}
		}
	}
	
	val printTest=new PrintTest
	
	
	setPrinterName(theService.getName)
	
	/*def showDialog {
    val filename = "";    
    val flavor=DocFlavor.STRING.TEXT_PLAIN        
    val defaultService = if(printerName==null) PrintServiceLookup.lookupDefaultPrintService() else
    		printServices.find(_.getName==printerName) match{
    	case Some(serv) =>serv
    	case None => println(printerName + " not found "); PrintServiceLookup.lookupDefaultPrintService()
    }
    //val pj=defaultService.createPrintJob
    /*printJob.printDialog
    theService=printJob.getPrintService
    val pageFormat=printJob.getPageFormat(null)
    println("pageFormat "+pageFormat.getWidth+" x "+pageFormat.getHeight+" x:"+pageFormat.getImageableX+" y:"+pageFormat.getImageableY+" orient:"+
    pageFormat.getOrientation)*/
    
    
    theService = ServiceUI.printDialog(null, 200, 200, printServices, defaultService, null, pras);
    if (theService != null) {
    	printerName=theService.getName
    	println("choosen service:"+theService.getName)
    	println("categories: \n"+theService.getSupportedAttributeCategories.mkString("\n"))
    	//println("flavors: "+theService.getSupportedDocFlavors.mkString("\n"))  
    	println("attributes:"+pras.toArray.map(ps=>ps.getClass+"| "+ps.getName+"| "+ps.toString).mkString("\n"))
    	println("allowedMediaSize:" + theService.getSupportedAttributeValues(classOf[javax.print.attribute.standard.MediaPrintableArea],
    		null,pras).asInstanceOf[Array[_]].mkString(", "))
    	
    	printTest.printString(pras, theService)
      /*val job = theService.createPrintJob()
      val listener = new PrintJobAdapter() {
        override def printDataTransferCompleted(e:PrintJobEvent ) = {
          println("Print done")
        }
      }
    	
      job.addPrintJobListener(listener);*/
      //FileInputStream fis = new FileInputStream(filename);
      val das = new HashDocAttributeSet();
      //val doc = new SimpleDoc("HELLO", flavor, das);
      //job.print(doc, pras);
      //Thread.sleep(10000);
    }
  }*/
  /*def pageDialog= {
  	if(theService==null) theService=PrintServiceLookup.lookupDefaultPrintService()
  	val dialog= new ServiceDialog(null,200,200,theService, null, pras, null.asInstanceOf[java.awt.Dialog])
  	dialog.setVisible(true)
	}*/
	
	
	
	title="Neue Ausgabe definieren:"
	var formList:Seq[FormDescription]=Seq.empty	
	val printTestBut=new Button("Testdruck")

	val cancelBut=new Button("Abbruch")
	val nextBut=new Button("Weiter ->")
	val portraitBut=new RadioButton("Hoch")
	val landscapeBut=new RadioButton("Quer")
	
	val pageFormGroup=new ButtonGroup(portraitBut,landscapeBut){
	 select(portraitBut)
	 listenTo(portraitBut,landscapeBut)
	 reactions+={
		 case ButtonClicked(`portraitBut`)|ButtonClicked(`landscapeBut`)=> pras.add(if(portraitBut.selected)
			 OrientationRequested.PORTRAIT else OrientationRequested.LANDSCAPE )
	 }
	}	
	
	var printerList=  printServices.map(s=>s.getName).toSeq
	val printerCombo=new ComboBox[String](printerList)
	val formListView=new ListView[FormDescription](){
		selection.intervalMode=ListView.IntervalMode.Single
		maximumSize=new Dimension(Short.MaxValue,0)
	}
	formListView.border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Ausdruckform auswählen:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
	
	val fieldColMod=new FieldColumnModel{
    	createColumn(0,"Name",90)
    	createColumn(1,"Beschreibung",200)
    	createColumn(2,"Wert",80)
	}
	
	val paramTabMod=new ParamTabMod	
	val paramTable=new Table {
		val stringEditor=new MultilineEditor(peer)	{
			def setEditorValue(value:Object) = if(value==null) "" else value.toString
		}
		model=paramTabMod
		peer.setAutoCreateColumnsFromModel(false)
    peer.setColumnModel(fieldColMod)
    selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None  
		autoResizeMode=Table.AutoResizeMode.Off
		peer.setDefaultEditor(classOf[Constant],stringEditor)
	}
	
	val paramScroller=new ScrollPane {
		viewportView=paramTable	
		border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Parameter angeben:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
	}
	
	preferredSize=new Dimension(500,450)
	
	val mainPanel=new BorderPanel(){
		formListView.background=background
		add(new BoxPanel(Orientation.Vertical ) {
			contents+=Swing.VStrut(10)+= formListView+=Swing.VStrut(10)+=paramScroller+=Swing.VStrut(10)+=
				new BoxPanel(Orientation.Horizontal) {
				contents+=new Label("Drucker:")+=printerCombo
				listenTo(printerCombo.selection)
				reactions+= {
					case SelectionChanged(e)=> setPrinterName( printerCombo.selection.item)
				}
			}+=Swing.VStrut(10)+=
			new BoxPanel(Orientation.Horizontal) {
				contents+=new Label("Papierformat: ")+=mediaCombo+=Swing.HStrut(20)+=portraitBut+=landscapeBut+=Swing.HStrut(20)
			}+=Swing.VStrut(10)+=
			new BoxPanel(Orientation.Horizontal) {
				contents+=new Label("Ausgabe-Schacht: ")+=trayCombo
			}+=Swing.VStrut(20)				
			
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+= cancelBut+=Swing.HStrut(40)+=printTestBut+=Swing.HGlue+=nextBut 
		},BorderPanel.Position.South)
		
		listenTo(nextBut,cancelBut,formListView.selection,/*printerBut,pageBut,*/printTestBut)
		reactions += {
			case ButtonClicked(`nextBut`)=> if(outputDefinedFunc!=null&& !formListView.selection.indices.isEmpty) {
				NewOutdefDialog.this.visible=false
				val paperSettings=lastSelectedMedia.mn.toString+(if(trayModel.getSize>0)"|"+trayCombo.selection.item.mt.toString else "")
				outputDefinedFunc(formListView.selection.indices.first,theService.getName,paperSettings,portraitBut.selected,
				 lastSelectedMedia.width.toInt,lastSelectedMedia.height.toInt,	paramTabMod.getParams)
			}
				
			case ButtonClicked(`cancelBut`)=>close
			case ListSelectionChanged(_,_ , isAdjusting)=> if(!isAdjusting&& !formListView.selection.indices.isEmpty){
				val ix=formListView.selection.indices.first				
				if(ix > -1) {
					val newForm=formList(ix)
					paramTabMod.loadForm( newForm)
				}
			}
			//case ButtonClicked(`printerBut`)=>showDialog
			//case ButtonClicked(`pageBut`)=>pageDialog
			case ButtonClicked(`printTestBut`)=>printTest.printString(pras, theService)
		}
	}	
	contents=mainPanel	
	
	def setPrinterName(newName:String)= {
		var defaultTrayIx= -1
		printerName=newName
		combosAdjusting=true
		mediaModel.removeAllElements
		trayModel.removeAllElements
		println("SetPrinterName:"+newName)
		//trayModel.addElement(AutoTray)
		theService=printServices.find(_.getName==printerName) match{
    	case Some(serv) =>serv
    	case None => println(printerName + " not found "); PrintServiceLookup.lookupDefaultPrintService()
		}	
		theService.getSupportedAttributeValues(classOf[Media],null,emptyAttributeSet).asInstanceOf[Array[_]].
		foreach(_ match {
			case n:MediaSizeName=>if(n.getValue<40 && !(n.toString.length>4&&n.toString.substring(0,3).equalsIgnoreCase("JIS")))  mediaModel.addElement(MediaMap.getMediaSize(n))
			case t:MediaTray => {
				if(t.toString=="Automatic-Feeder") defaultTrayIx=trayModel.getSize
				trayModel.addElement(TrayMap.getMediaTray(t))
				
			}
			case p:MediaName=> println("*** MediaName:"+p)
			case e=> println("*** Unknown element:"+e)
		})
		if(lastSelectedMedia!=null && theService.isAttributeValueSupported(lastSelectedMedia.mn, null, emptyAttributeSet))
			 mediaCombo.selection.index=mediaModel.getIndexOf(lastSelectedMedia)
		 else {
			println("lastSelMedia not supported "+lastSelectedMedia)
			val defMed=theService.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName]
			val newMap=MediaMap.getMediaSize(defMed)
			setMediaWrapper(newMap)
			mediaCombo.selection.index=mediaModel.getIndexOf(lastSelectedMedia)
		}
		if(trayModel.getSize>0&&defaultTrayIx> -1) {
			trayCombo.selection.index_=(defaultTrayIx)
			pras.add(trayModel.getElementAt(defaultTrayIx).asInstanceOf[MediaTrayWrapper].altValue)
		}
			
		combosAdjusting=false
		//checkPrintableArea
	}
	
	def getPrintableArea= {
		val pList=theService.getSupportedAttributeValues(classOf[javax.print.attribute.standard.MediaPrintableArea],
    		null,pras).asInstanceOf[Array[MediaPrintableArea]]	
		if(pList.isEmpty) throw new IllegalArgumentException("Cant find Supported PrintableArea ")
		else pList.first
	}
	
	def getCurrentForm = {
		formListView.selection.items.first
	}
	
	def getPageFormat= {
		printJob.setPrintService(theService)
		printJob.getPageFormat(pras)		
	}
	
	def print(title:String)= {
		printJob.setJobName(title)
		printJob.print(pras)
	}
	
	def setMediaWrapper(newWrapper:MediaSizeWrapper)= {
		pras.remove(classOf[MediaPrintableArea])
		pras.add(newWrapper.mn)		
		//pras.remove(classOf[MediaPrintableArea])
		println("set wrapper class:"+newWrapper.mediaSize .getClass+" "+newWrapper.mn.getClass)
		println("pras:" +pras.toArray.mkString("\n"))
		//println("service:"+theService)
		lastSelectedMedia=newWrapper
		//checkPrintableArea
		/*if(allowedSizes!=null && allowedSizes.size>0) {
			pras.add(allowedSizes.last)
		}*/
	}
	
	def showDialog (noutputDefinedFunc:(Int,String,String,Boolean,Int,Int,Seq[(String,Constant)])=>Unit) = {
		outputDefinedFunc=noutputDefinedFunc
		visible=true
	}
	
	def showEditDialog(noutputDefinedFunc:(Int,String,String,Boolean,Int,Int,Seq[(String,Constant)])=>Unit,odef:OutputDefinition) = {
		loadOutDefSettings(odef)
		showDialog(noutputDefinedFunc)
	}
	
	def loadForms(newList:Seq[FormDescription])= {		
		formList=newList
		formListView.listData=newList
		formListView.selection.indices+=0
	}
	
	def loadOutDefSettings(odef:OutputDefinition) = {
		//println("load OutDefSettings "+odef)
		val sIx=printerList.indexOf(odef.printer)
		if(sIx<0) println("cant find printer "+odef.printer)
		else {
			printerCombo.selection.index=sIx
			//theService=printServices(sIx)
			//setPrinterName(odef.printer)
		}
		val paperS=odef.paperSetting .split('|')
		val paged=paperS(0)
		combosAdjusting=true
		theService.getSupportedAttributeValues(classOf[Media],null,emptyAttributeSet).asInstanceOf[Array[Media]].
			find(_.toString==paged) match {
			case Some(media)=> {				
				val wrapper=MediaMap.getMediaSize(media.asInstanceOf[MediaSizeName])				
				val ix=mediaModel.getIndexOf(wrapper)
				//println("Found Media: "+media+ " "+wrapper+" ix:"+ix)
				if(ix> -1) mediaCombo.selection.index=ix
				setMediaWrapper(wrapper)
			}
			case None => System.err.println("Cant find Media :"+paged)
		}		
		if(paperS.size>1) { // tray
			val trayName=paperS(1)
			theService.getSupportedAttributeValues(classOf[Media],null,emptyAttributeSet).asInstanceOf[Array[Media]].
			find(_.toString==trayName) match {
			case Some(tray)=> {				
				val wrapper=TrayMap.getMediaTray(tray.asInstanceOf[MediaTray])
				val ix=trayModel.getIndexOf(wrapper)
				//println("Found tray "+tray+ " ix:"+ix)
				if(ix> -1) trayCombo.selection.index=ix
				pras.add(wrapper.altValue)
			}
			case None =>System.err.println("Cant find Tray :"+trayName)
		}
		}
		if(odef.portrait )portraitBut.selected=true else landscapeBut.selected=true
		pras.add(if(odef.portrait) OrientationRequested.PORTRAIT else OrientationRequested.LANDSCAPE )
		// load params
		println("load params:"+odef.paramValues .mkString(";"))
		combosAdjusting=false
		def findParamValue(pname:String)= {
			println("Find param Value:"+pname)
			odef.paramValues.find(_._1 ==pname) match {
				case Some(param)=> println("found:"+param._2); param._2 
				case None => EMPTY_EX
			}
		}
		val fix=formList.findIndexOf(_.inst ==odef.formInst )
		if(fix> -1) { 
			formListView.selection.indices+=fix			
			paramTabMod.loadForm(formList(fix),findParamValue)
		}
		else System.err.println("can't find form :"+odef.formInst)		
	}
	
	peer.addWindowListener (new WindowAdapter(){		
		override def windowClosing(e:WindowEvent)= {
			println("Closing ")
		}
	})
}

class PrintTest extends java.awt.print.Printable {  
     
    def printString(aset:PrintRequestAttributeSet,service:PrintService)= { 
    	println("print pras:" +aset.toArray.mkString("\n"))     
        val pj = PrinterJob.getPrinterJob()        
        pj.setPrintable(this) 
        try {
            pj.setPrintService(service)            
            pj.print(aset);
        } catch  {
        	case pe:PrinterException => System.err.println(pe)
        }
    }
 
    def print(g:Graphics,pf: PageFormat,pageIndex:Int)= 
    	if(pageIndex==0){
        val g2d= g.asInstanceOf[Graphics2D];
        g2d.translate(pf.getImageableX(), pf.getImageableY());
        g.drawString(String.valueOf("Test druck "+pf.getImageableX), 10, 10);        
        g.drawLine(0,0,pf.getImageableWidth.toInt,pf.getImageableHeight.toInt)
        g.drawRect(0,0,pf.getImageableWidth.toInt,pf.getImageableHeight.toInt)
        Printable.PAGE_EXISTS        
    	}
    	else Printable.NO_SUCH_PAGE
}

