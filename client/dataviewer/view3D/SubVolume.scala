/**
 * Author: Peter Started:25.05.2011
 */
package client.dataviewer.view3D

import definition.data.{Reference,InstanceData,OwnerReference}
import client.comm._
import definition.comm._
import definition.expression.{Line3D,VectorConstant}
import definition.typ.AllClasses
import java.io.DataInput
import java.awt.{Color,Font}
import javax.media.j3d._
import javax.vecmath._

/**
 * 
 */
class SubVolume(workArea:WorkArea,ndata:InstanceData,parentVolume:Option[SubVolume]) extends DBBranchGroup(ndata.ref) {
  val lock=new Object
	var subVolumes:Seq[SubVolume]=Seq.empty
	var borderPlanes= collection.mutable.ArrayBuffer[PlaneElement]()
	var axisPlanes:Seq[PlaneElement]=Seq.empty
	
	var selectedAxisPlane:Option[PlaneElement]=None
	
	val name=ndata.fieldValue(0).toString
	val color=ndata.fieldValue(1).toInt
	
	var volSubsID:Int=0
	var axisPlaneSubsID:Int=0
	var borderPlaneSubsID:Int=0
	
	var numBorderPlanesToLoad:Int=0
	
	val axisPlaneListeners=new collection.mutable.HashMap[Reference,WorkArea.planeUpdateFunc]()
	
	var volumeShape:Option[VolumeBranchGroup]=None
	
	var axisPlanesBranchGroup:Option[AxisPlaneBranchGroup]=None
	
	var selected=false
  
  
	def loadData():Unit = lock.synchronized{
  	axisPlaneSubsID= ClientQueryManager.createFactSubscription[PlaneElement](ndata.ref,2,PlaneFactory){
			(typ:NotificationType.Value,data:IndexedSeq[PlaneElement])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData =>{
							axisPlanes=data
							//println("axis send data"+data.size)
							axisLoaded()
						}
						case NotificationType.FieldChanged => {    					
							val searchRef=data(0).ref
							val ix=axisPlanes.findIndexOf(_.ref == searchRef)
							if(ix<0) error("can't find changed AxisPlane "+searchRef)
							else {    						
								axisPlanes=axisPlanes.updated(ix,data(0))
								if(axisPlaneListeners.contains(data(0).ref))
									axisPlaneListeners(data(0).ref).apply(data(0))
								axisUpdated()
							}
						}
						case NotificationType.instanceRemoved => {
							val searchRef=data(0).ref
							val ix=axisPlanes.findIndexOf(_.ref == searchRef)
							if(ix<0) error("can't find deleted AxisPlane "+searchRef)
							else {    						
								axisPlanes=axisPlanes.take(ix)++axisPlanes.drop(ix+1)
								workArea.editor.shapeDeleted(searchRef)
								axisUpdated()
							}
						}
						case NotificationType.childAdded => {
							//println(" axis added ")
							axisPlanes=axisPlanes:+data(0)
							
							axisUpdated()
						}
					}
				}
			}  	
  	}		
	}
  
  def axisLoaded() = {
  	// load sub Volumes
  	//println("axis loaded "+name)
  	volSubsID=ClientQueryManager.createSubscription(ndata.ref,0){
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData =>{
							println("SubVolumes "+data.mkString(","))
							subVolumes=data map (new SubVolume(workArea,_,Some(this)))
							subVolumes foreach (s=> {
								s.loadData
								s.compile
								addChild(s)})
						}
						case NotificationType.FieldChanged => {
							val ix=subVolumes.indexWhere(_.ref==data(0).ref)
							if(ix> -1) {
								removeChild(subVolumes(ix))
								val newVol=new SubVolume(workArea,data(0),Some(this))
								subVolumes= subVolumes.updated(ix,newVol)
								newVol.loadData
								newVol.compile
								addChild(newVol)
							}
							else throw new IllegalArgumentException("Volume changed cant find "+data(0).ref)
						}
						case NotificationType.childAdded => {
							val newVol=new SubVolume(workArea,data(0),Some(this))
							subVolumes=subVolumes:+newVol
							newVol.loadData
							addChild(newVol)
						}
						case NotificationType.instanceRemoved=> {
							val ix=subVolumes.indexWhere(_.ref==data(0).ref)
							if(ix> -1){
								subVolumes(ix).shutDown
								removeChild(subVolumes(ix))
								subVolumes= subVolumes.take(ix)++subVolumes.drop(ix+1)
								workArea.editor.shapeDeleted(data(0).ref)
							}
							else throw new IllegalArgumentException("Volume deleted cant find "+data(0).ref)							
						}
					}
				}
			}
		}
  	
  	axisUpdated()
  	// load border plane references only, get objects from work Area 
  	borderPlaneSubsID=ClientQueryManager.createSubscription(ndata.ref,1){
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> ClientQueryManager.runInPool{  
				lock.synchronized {
					typ match {
						case NotificationType.sendData =>{							
							numBorderPlanesToLoad=data.size
							//println("Border NumtoLoad:"+data.size)
							for(d <-data){
								workArea.registerPlaneListener(d.ref ,(planeElement)=> lock.synchronized{
									//println("borderPlan notify "+planeElement)
									val ix=borderPlanes.indexWhere(_.ref==d.ref)
									if(ix> -1)borderPlanes(ix)=planeElement
									else borderPlanes+=planeElement
									if(borderPlanes.size==numBorderPlanesToLoad) updateGeometry
								})
							}
						}
						
						case _ =>
					}
				}
			}
  	}
  	
  }
  
  def axisUpdated() = lock.synchronized{
  	//println("axis updated volumeShape:"+volumeShape+" axisGroup:"+axisPlanesBranchGroup)
  	for(a<-axisPlanesBranchGroup) 
  		removeChild(a)  		
  	axisPlanesBranchGroup=Some(new AxisPlaneBranchGroup(axisPlanes,selectedAxisPlane))
  	for(p<-volumeShape) {
  		axisPlanesBranchGroup.get.setPrism(p)
  		axisPlanesBranchGroup.get.compile
  		addChild(axisPlanesBranchGroup.get)
  	}  	
  }
  
  def shutDown= lock.synchronized{
  	if(getParent!=null){
  		getParent.asInstanceOf[BranchGroup].removeChild(this)
  		for(a<-axisPlanesBranchGroup)
  			removeChild(a)
  		for(v<-volumeShape)
  			removeChild(v)
  	}
  	if(volSubsID>0) {  		
  		ClientQueryManager.removeSubscription(volSubsID)
  		volSubsID==0
  	}
  	if(axisPlaneSubsID>0){
  		ClientQueryManager.removeSubscription(axisPlaneSubsID)
  		axisPlaneSubsID==0
  	}
  	if(borderPlaneSubsID>0){
  		ClientQueryManager.removeSubscription(borderPlaneSubsID)
  		borderPlaneSubsID==0
  	}
  	selected=false
  }
  
  /** wich SubVolume holds the designated plane
	 * 
	 * @param planeRef reference of the plane
	 */
	def getPlaneHolder(planeRef:Reference):Option[SubVolume]= lock.synchronized{
		//if(borderPlanes.exists(_.ref==planeRef)) Some (this)
		if(axisPlanes.exists(_.ref==planeRef)) return Some(this)
		for(v<-subVolumes) v.getPlaneHolder(planeRef) match {
			case a@Some(_)=> return a
			case _ =>
		}
		None
	}
	
	def registerAxisPlaneListener(ref:Reference,ufunc:WorkArea.planeUpdateFunc)= lock.synchronized{
		axisPlaneListeners(ref)=ufunc
		axisPlanes.find(_.ref==ref) match {
			case Some(a)=> ufunc(a)
			case _=> throw new IllegalArgumentException("Cant find Axis Plane "+ref + " in "+ref)
		}		
	}
	
	def getPlane(ref:Reference):PlaneElement= lock.synchronized{
		borderPlanes.find(_.ref==ref) getOrElse( getAxisPlane(ref) match {
			case Some(s)=> s
			case _=> throw new IllegalArgumentException("cant find Plane "+ref)
		})
	}
	
	def getAxisPlane(ref:Reference):Option[PlaneElement] = {
		axisPlanes.find(_.ref==ref) match {
			case a@Some(p_)=>a
			case _=> for(s<- subVolumes) 
				s.getAxisPlane(ref) match {
				case a@Some(_)=>return a
				case _=> 
			}
		}
		return None
	}
	
	def getSubVolume(sref:Reference):Option[SubVolume] = {
		if(ref==sref)return Option(this)
		else for(s<-subVolumes){
			s.getSubVolume(sref) match {
				case a@Some(_)=> return a
				case _=>
			}
		}
		/*subVolumes.find(_.ref == ref) match {
			case s@Some(_) => s
		  case _ => for(s<-subVolumes)
		  	s.getSubVolume(ref) match {
		  	case a@Some(_)=> return a
		  	case _ => 
		  }
		}*/
		return None
	}
	
	def updateGeometry() = lock.synchronized{
		
		for(a<-axisPlanesBranchGroup) 
			removeChild(a)
		for(c <-volumeShape){			
			removeChild(c)
		}
		volumeShape=Some(new VolumeBranchGroup(ndata.ref,ndata.owners(0),name,borderPlanes,color,selected,
			parentVolume .flatMap(_.volumeShape)))
		
		/*volumeShape=Some(new VolumeBranchGroup(ndata.ref,ndata.owners(0),name,borderPlanes,color,selected,
			parentVolume match { case Some(v)=>v.volumeShape;case _=> None}))*/
		//println("updateGeometry "+name+" "+axisPlanesBranchGroup)
		for(a<-axisPlanesBranchGroup){
			if(a.volPrism ==None)
				a.setPrism(volumeShape.get)
			else {
				axisPlanesBranchGroup=Some(new AxisPlaneBranchGroup(axisPlanes,selectedAxisPlane))
				axisPlanesBranchGroup.get.setPrism(volumeShape.get)
			}
			axisPlanesBranchGroup.get.compile
			addChild(axisPlanesBranchGroup.get)
		}			
		volumeShape.get.compile
		addChild(volumeShape.get)			
	}
	
	// *********************************** SELECT ********************
	
	/** selects the shape if its part of this subvolume
	 * 
	 * @param shape
	 * @return true if the shape was found
	 */
	def selectShape(shape:RefShape)= lock.synchronized{
		//println("Select "+shape)
		shape match {
			case a:AxisPlane => 
			case a:AxisShape => getPlaneHolder(shape.ref) match {
				case Some(holder)=> holder.selectAxis(shape.ref)		 
				case None => throw new IllegalArgumentException("Select Cant find holder for plane:"+shape.ref)
			}
			case f:FilledVolumeShape => 
			case v:VolumeShape => getSubVolume(v.ref) match {
				case Some(volume) => volume.selectVolume()
				case None => throw new IllegalArgumentException("Select cant find volume "+shape.ref)
			}
			
		}
	 		
	}
	
	def selectAxis(nref:Reference)= {		
	  selectedAxisPlane=axisPlanes.find(_.ref ==nref) 
	  //println("axis selected:"+selectedAxisPlane)
		axisUpdated()
	}
	
	def deselectAxis()= {
		selectedAxisPlane=None
		axisUpdated()
	}
	
	def selectVolume()= {
		selected=true
		updateGeometry()
	}
	
	def deselectVolume() = {
		selected=false
		updateGeometry()
	}
	
	def deselectShape(shape:RefShape) = lock.synchronized{
		shape match {
			case a:AxisPlane => getPlaneHolder(shape.ref) match {
				case Some(holder)=> holder.deselectAxis()		 
				case None => println("Deselect Cant find holder for plane:"+shape.ref)
			}
			case a:AxisShape => getPlaneHolder(shape.ref) match {
				case Some(holder)=> holder.deselectAxis()		 
				case None => println("Deselect Cant find holder for plane:"+shape.ref)
			}
			case v:VolumeShape => getSubVolume(v.ref) match {
				case Some(volume) => volume.deselectVolume()
				case None => println("Deselect cant find volume "+shape.ref)
			}
			case v:FilledVolumeShape => getSubVolume(v.ref) match {
				case Some(volume) => volume.deselectVolume()
				case None => println("Deselect cant find volume "+shape.ref)
			}
			
	 }
	}
	
	
}


object SubVolume {
	/** creates a new SubVolume with the given data
	 * 
	 * @param workArea
	 * @param parentRef
	 * @param propfield
	 * @param borderPlanes
	 */
	def createSubVolume(workArea:WorkArea,parentRef:Reference,propField:Byte,borderPlanes:Seq[PlaneElement]):Reference= {
		val inst=ClientQueryManager.createInstance(workArea.volumeType,Array(new OwnerReference(propField,parentRef)))
		val instRef=new Reference(workArea.volumeType,inst)
		for(pl <-borderPlanes) {
			ClientQueryManager.secondUseInstances(List(pl.ref ), pl.owners(0),new OwnerReference(1.toByte,instRef), -1)
		}		
		instRef
	}	
	
	def getCenterPoint(pointList:Seq[Point3d]):Point3d= {
		val ret=new Point3d
		if(!pointList.isEmpty) {
			pointList.foreach(a=> {
				ret.add(a)
			})
			ret.scale(1f/pointList.size.toFloat)
		}
		ret		
	}
   	
	val axisPlanePolygonAttributes=new PolygonAttributes(PolygonAttributes.POLYGON_FILL,PolygonAttributes.CULL_NONE,0f )
  //val axisPlanePolygonAttributes=new PolygonAttributes(PolygonAttributes.POLYGON_FILL,PolygonAttributes.CULL_NONE,0f )

}

class VolumeShape(val prism:VolumeBranchGroup) extends RefShape(prism.ref,prism.oref) {
	setGeometry(prism.lineStrips)
	setAppearance(prism.appearance)
}

class FilledVolumeShape(prism:VolumeBranchGroup) extends RefShape(prism.ref,prism.oref) {
	val appear=new Appearance
	appear.setColoringAttributes(prism.colorA)
	appear.setTransparencyAttributes(new TransparencyAttributes(TransparencyAttributes.FASTEST,0.85f))
	//appear.setPolygonAttributes(SubVolume.axisPlanePolygonAttributes)
	setAppearance(appear)
	val topBotCoords=prism.inside.topPoints.reverse++prism.inside.bottomPoints
	
	val sideCoords= (for(i<- prism.inside.topPoints.indices;val sec=if(i<prism.inside.topPoints.size-1)i+1 else 0)
	  yield List(prism.inside.topPoints(i),prism.inside.topPoints(sec),prism.inside.bottomPoints(sec),prism.inside.bottomPoints(i))).flatten
	val coords=topBotCoords++sideCoords
	val lengthArray=List(prism.inside.topPoints.size,prism.inside.bottomPoints.size)++(for(i<-prism.inside.topPoints.indices) yield (4))
	//println("FilledVolume "+coords)
	//val sideFans=
	val fanArray=new TriangleFanArray(coords.size,GeometryArray.COORDINATES,lengthArray.toArray)
	fanArray.setCoordinates(0,coords.toArray)
	
	setGeometry(fanArray)
}

class VolumeBranchGroup (nref:Reference,val oref:OwnerReference,val name:String,val planeList:Seq[PlaneElement],
	val ncolor:Int,val selected:Boolean,parentGroup:Option[VolumeBranchGroup]) extends  DBBranchGroup(nref) {
	val offset=0.1f
	lazy val geometryPrism=new DataPrism(nref,planeList)
	
	lazy val insidePlanes:Seq[PlaneElement]=parentGroup match {
		case Some(p)=>{ 
			planeList.map(plane=>{
				val fitIx=p.planeList .indexWhere(_==plane)
				if(fitIx> -1) p.insidePlanes(fitIx).createOffsetPlane(geometryPrism.centerP,offset)
				else plane.createOffsetPlane(geometryPrism.centerP,offset)
			})
		}			
		case _=>  planeList.map(_.createOffsetPlane(geometryPrism.centerP,offset).asInstanceOf[PlaneElement])
	}
	
	lazy val inside=new DataPrism(nref,insidePlanes)
	
	lazy val updownLines=(for(i<-geometryPrism.wallPlanes.indices) yield List(inside.topPoints(i),inside.bottomPoints(i))).flatten	
	lazy val lineCoords=inside.topPoints++List(inside.topPoints(0))++inside.bottomPoints++List(inside.bottomPoints(0))++updownLines
	lazy val lineStrips=new LineStripArray((geometryPrism.wallPlanes.size*2+1)*2,GeometryArray.COORDINATES,
		Array(geometryPrism.wallPlanes.size+1,geometryPrism.wallPlanes.size+1)++Array[Int]().padTo(geometryPrism.wallPlanes.size,2))
		
	
	val appearance=new Appearance()
	val color=new Color(ncolor)
	val colorf=new Color3f(color)
	val colorA=new ColoringAttributes(colorf,ColoringAttributes.NICEST)
	val transA=new TransparencyAttributes(TransparencyAttributes.NICEST,0.5f)
	appearance.setColoringAttributes(colorA)
	appearance.setTransparencyAttributes(transA)
	
	def init= {
		lineStrips.setCoordinates(0,lineCoords.toArray)
		lazy val boxShape=new VolumeShape(this)
		addChild(boxShape)
		if(selected) {
			val fillShape=new FilledVolumeShape(this)
			addChild(fillShape)
		}
		val font3d = new Font3D(new Font("Helvetica", Font.PLAIN, 1),new FontExtrusion());	
		addChild(WorkArea.createAxisText(name,geometryPrism.centerPoint,font3d,colorf,0.5f))
	}
	init
	
}




class AxisPlaneBranchGroup(axisPlaneList:Seq[PlaneElement],selPlane:Option[PlaneElement]) extends DBBranchGroup(null) {
	var volPrism:Option[VolumeBranchGroup]=None
	var axisShapes:Seq[AxisShape]=Seq.empty
	
	def createGeometry= {
		//println("axis create Geometry "+axisPlaneList.mkString(","))		
		val font3d = new Font3D(new Font("Helvetica", Font.PLAIN, 1),new FontExtrusion());

		for(volPris<-volPrism) {		
			axisShapes=axisPlaneList map (new AxisShape(_,volPris))
			for(a <-axisShapes) {
				addChild(WorkArea.createAxisText(a.plane.name,a.centerPoint,font3d,a.colorf,0.4f))
				addChild(a)
			}
			for(sel<-selPlane) {
				//println("try create axisPlane")
				axisShapes.find(_.plane.ref==sel.ref) match {
					case Some(shape) => {
						val selectPlane=new AxisPlane(shape)
						addChild(selectPlane)
						addChild(new AxisNormVector(shape))
					}
					case None => println("Cant find selected Shape "+sel + "!!!")
				}
				
			}
		}	
		
	}
	
	
	def setPrism(newPrism:VolumeBranchGroup)= {
		/*for(v<-volPrism) 
			removeAllChildren*/
		volPrism=Some(newPrism)
		createGeometry
	}
}