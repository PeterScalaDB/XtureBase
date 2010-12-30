/**
 * Author: Peter Started:04.09.2010
 */
package client.comm

import definition.data._
import definition.typ._
import definition.comm._
import definition.expression._
import java.util.concurrent._
import java.io._
import scala.collection.immutable.IndexedSeq
import javax.swing.SwingUtilities
import client.dialog.DialogManager

/** manages Queries and Subscriptions
 * 
 */

trait StepListReader{
	def loadStepList(list:Seq[TransStepData])
}

trait ErrorListener{
	def printError(errorText:String)
}

object ClientQueryManager {
	//type InstArrayFunc=(Array[InstanceData])=> Unit
	type UpdateFunc=(NotificationType.Value,IndexedSeq[InstanceData])=>Unit
	//type PosUpdateFunc=(NotificationType.Value,IndexedSeq[InstanceData],Int)=>Unit
	
	type FactUpdateFunc[T<: Referencable]=(NotificationType.Value,IndexedSeq[T])=>Unit
	
	val errorListeners=new collection.mutable.HashSet[ErrorListener]()	
	
	trait Subscriber
	case class SimpleSubscriber(func: UpdateFunc) extends Subscriber
	//case class PositionSubscriber(func: PosUpdateFunc) extends Subscriber
	case class FactSubscriber[T<: Referencable](factory:SubscriptionFactory[T],func: FactUpdateFunc[T]) extends Subscriber
	//case class PathSubscriber(func:PathUpdateFunc)
	
	private val queryQueue = new SynchronousQueue[IndexedSeq[InstanceData]]()	
	private val newSubscriberQueue=new ArrayBlockingQueue[Subscriber](3)
	private val subscriptionMap=new ConcurrentHashMap[Int,Subscriber]()
	
	private var stepListReader:StepListReader=null
	
	private val commandResultQueue = new SynchronousQueue[Option[Constant]]()
	private val subscriptionAcceptQueue = new SynchronousQueue[Int]()
	//private val pathSubsAcceptQueue=new SynchronousQueue[Int]()
	
	private var sock:ClientSocket=null
	private val myPool=Executors.newCachedThreadPool() 
	private var isSetup=false
	private var setupListenerMap=collection.mutable.HashSet[Function0[Unit]]()
	private var afterSetupListeners=collection.mutable.HashSet[Function0[Unit]]()
	private val storeSettingsListenerMap= collection.mutable.HashSet[Function0[Unit]]()
	
	def setClientSocket(newSock:ClientSocket) = {
		sock=newSock
		// register Handler
		sock.registerCommandHandler(ServerCommands.sendQueryResponse)(handleQueryResults)
		sock.registerCommandHandler(ServerCommands.acceptSubscription)(handleAcceptSubscription)	
		sock.registerCommandHandler(ServerCommands.sendSubscriptionNotification )(handleSubsNotifications)
		sock.registerCommandHandler(ServerCommands.sendCommandResponse )(handleCommandResponse)
		sock.registerCommandHandler(ServerCommands.lockForUndo )(lockForUndo)
		sock.registerCommandHandler(ServerCommands.releaseUndoLock )(releaseUndoLock)
		sock.registerCommandHandler(ServerCommands.sendUndoInformation )(sendUndoInformation)
		sock.registerCommandHandler(ServerCommands.askEnquiry )(askEnquiry)
	}
		
		
	def queryInstance(ref:Reference,propertyField:Byte):IndexedSeq[InstanceData] = 	{		
		sock.sendData(ClientCommands.queryInstance ) {out =>			
			//System.out.println("Sending Query request "+ref + " "+Thread.currentThread)
			ref.write(out)
			out.writeByte(propertyField)
		}
		queryQueue.take
	}
	
	/** creates a new subscription
	 * @param updateFunc a function to be called when values are updated
	 * @return the subscriptionID
	 * 	
	 */
	def createSubscription(parentRef:Reference,propertyField:Byte)(updateFunc: UpdateFunc):Int = {		
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( SimpleSubscriber(updateFunc))
			//System.out.println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}
	/** creates a new subscription that wants to get position information when instances are moved
	 * 
	 */
	/*def createPosSubscription(parentRef:Reference,propertyField:Byte)(updateFunc: PosUpdateFunc):Int = {		
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( PositionSubscriber(updateFunc))
			//System.out.println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}*/
	
	
	/** creates a Subscription with a Factory
	 * 
	 */
	def createFactSubscription[T <:Referencable](parentRef:Reference,propertyField:Byte,factory:SubscriptionFactory[T])
	(updateFunc: FactUpdateFunc[T]):Int = {		
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( FactSubscriber(factory,updateFunc))
			//System.out.println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}
	
	def changeSubscription(subsID:Int,newParent:Reference,newPropField:Byte) = {
		sock.sendData(ClientCommands.changeSubscription ) {out =>
			out.writeInt(subsID)
			newParent.write(out)
			out.writeByte(newPropField)
		}
	}
	
	def createPathSubscription(path:Seq[Reference])(updateFunc:UpdateFunc):Int = {		
		sock.sendData(ClientCommands.startPathSubscription ) {out =>
			newSubscriberQueue.add( SimpleSubscriber(updateFunc))
			out.writeInt(path.size)
			for(el <-path) el.write(out)
		}
		subscriptionAcceptQueue.take()
	}
	
	def pathSubs_addPathElement(subsID:Int, childRef:Reference) = {
		sock.sendData(ClientCommands.pathSubs_openChild ) { out =>
			out.writeInt(subsID)
			childRef.write(out)
		}
	}
	
	/** changes the subscription only to the remaining elements
	 * @param newPathPos the number of the element that should be the last one starting with 0
	 */
	def pathSubs_jumpUp(subsID:Int,newPathPos:Int) = {
		sock.sendData(ClientCommands.pathSubs_jumpUp  ) { out =>
			out.writeInt(subsID)
			out.writeInt(newPathPos)
		}
	}
	
	def pathSubs_changePath(subsID:Int,newPath:Seq[Reference]) = {
		sock.sendData(ClientCommands.pathSubs_jumpUp  ) { out =>
			out.writeInt(subsID)
			out.writeInt(newPath.size)
			for(p <-newPath) p.write(out)
		}
	}
	
	
	def removeSubscription(subsID:Int) = {
		//Thread.dumpStack
		sock.sendData(ClientCommands.stopSubscription ) {out =>
			out.writeInt(subsID)			
		}
		if(subscriptionMap.containsKey(subsID)) subscriptionMap.remove(subsID)		
		else System.out.println("ERROR: subscription "+subsID+" not found when removing")
	}
	
	
	def pauseSubscription(subsID:Int) = {
		//Thread.dumpStack
		sock.sendData(ClientCommands.pauseSubscription ) {out =>
			out.writeInt(subsID)			
		}		
	}
	
	
	def writeInstanceField(ref:Reference,fieldNr:Byte,newValue:Expression) = {
		sock.sendData(ClientCommands.writeField ) { out =>
			ref.write(out)
			out.writeByte(fieldNr)
			newValue.write(out)
		}
		commandResultQueue.take()			
	}
	
	def writeInstancesField(refs:Seq[Referencable],fieldNr:Byte,newValue:Expression) = {
		sock.sendData(ClientCommands.writeMultiFields ) { out =>
		  out.writeInt(refs.size)
		  for(inst<-refs) inst.ref.write(out)
			out.writeByte(fieldNr)
			newValue.write(out)
		}
		commandResultQueue.take()			
	}
		
	
	/** creates an instance and returns the instanceID
	 * @param classType the typeID of the new instance
	 * @param owners the owners of the new instance
	 * @return the ID of the new class
	 */
	def createInstance(classType:Int,owners:Array[OwnerReference]):Int = {
		sock.sendData(ClientCommands.createInstance ) { out =>
		   out.writeInt(classType)
		   out.writeInt(owners.size)
		   for(owner <-owners)
		  	 owner.write(out)			
		}
		//System.out.println("create "+Thread.currentThread.getName)
		commandResultQueue.take() match {
			case Some(const) => const.toInt
			case None => throw new IllegalArgumentException("no instance ID returned when creating type "+classType)
		}
	}
	
	
	def copyInstances(refList:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Int = {
		copyOrMove(refList,fromOwner,toOwner,atPos,ClientCommands.copyInstances )
	}
	
	def moveInstances(refList:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Int = {
		copyOrMove(refList,fromOwner,toOwner,atPos,ClientCommands.moveInstances )
	}
	
	def copyOrMove(refList:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int,command:ClientCommands.Value)= {		
		sock.sendData(command ) { out =>
		  out.writeShort(refList.size)
		  for(ref <-refList) ref.write(out)
			fromOwner.write(out)
			toOwner.write(out)
			out.writeInt(atPos)
		}
		commandResultQueue.take() match {			
			case Some(const) =>const.toInt
			case None => throw new IllegalArgumentException("no instance ID returned when copying instances "+refList.mkString)
		}
	}
	
	def secondUseInstances(refList:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int) = {
		sock.sendData(ClientCommands.secondUseInstances  ) { out =>
		  out.writeShort(refList.size)
		  for(ref <-refList) ref.write(out)
			fromOwner.write(out)
			toOwner.write(out)
			out.writeInt(atPos)
		}
		commandResultQueue.take() match {			
			case Some(const) =>const.toInt
			case None => throw new IllegalArgumentException("no instance ID returned when copying instances "+refList.mkString)
		}
	}
	
	
	def deleteInstance(ref:Reference,fromOwner:OwnerReference) = {
		sock.sendData(ClientCommands.deleteInstance ) { out =>
			ref.write(out)
			fromOwner.write(out)
		}
		commandResultQueue.take() 
	}
	
	/** sends a notification to the server to execute the given action with the given parameters
	 * @instList list of instances that should be modified. For a Create Action, the list of parents
	 * @actionName name of the action
	 * @params the parameter values for the action
	 * @createAction is this a Create Action or Modification Action
	 * 
	 */
	def executeAction(owner:OwnerReference,instList:Seq[Referencable],actionName:String,params:Seq[(String,Constant)])= {		
		sock.sendData( ClientCommands.executeAction) { out =>
			out.writeInt(instList.size)
			instList foreach(_.ref.write(out))
			out.writeUTF(actionName)
			out.writeInt(params.size)
			params foreach((x) => {out.writeUTF(x._1); x._2 .write(out)})
			owner.write(out)
		}
		commandResultQueue.take() 	
	}
	
	def executeCreateAction(parentList:Seq[Referencable],newType:Int,propField:Byte,actionName:String,params:Seq[(String,Constant)])= {
		//System.out.println("execute create newType:" + newType)
		sock.sendData(ClientCommands.executeCreateAction) { out =>
			out.writeInt(parentList.size)
			parentList foreach(_.ref.write(out))
			out.writeInt(newType)
			out.writeByte(propField)
			out.writeUTF(actionName)
			out.writeInt(params.size)
			params foreach((x) => {out.writeUTF(x._1); x._2 .write(out)})
		}
		commandResultQueue.take() 	
	}
	
	
	// ************************************* Internal routines *************************************
	
	private def readList(in:DataInputStream):IndexedSeq[InstanceData] = {
			val numData=in.readInt
			for(i <- 0 until numData) yield InstanceData.readWithChildInfo(Reference(in), in)			
		}
	
	private def readListWithFactory[T <: Referencable](in:DataInputStream,factory:SubscriptionFactory[T]):IndexedSeq[T] = {
			val numData=in.readInt
			for(i <- 0 until numData) yield factory.createObject(Reference(in), in)			
		}
	
	private def handleQueryResults(in:DataInputStream) = 	{
		val data=readList(in)
		//System.out.println("Handling Query result data size:"+data.size+ " "+Thread.currentThread)
		
		queryQueue.put(data)
		
	}
	
	
	private def handleAcceptSubscription(in:DataInputStream) = {
		val subsID:Int=in.readInt		
		//System.out.println("Handling accept subs subsID:"+subsID)
		
		val subs:Subscriber=newSubscriberQueue.take()			
		subscriptionMap.put(subsID,subs)
		subscriptionAcceptQueue.put(subsID)
		subs match {			
			case a:SimpleSubscriber =>{
				val data=readList(in)
				a.func(NotificationType.sendData,data)
			}
			/*case c:PositionSubscriber =>{
				val data=readList(in)
				c.func(NotificationType.sendData,data,-1)
			}*/
			case b:FactSubscriber[_] => {
				val data=readListWithFactory(in,b.factory)
				b.func(NotificationType.sendData,data)
			}
		}			
	}
	
	
	
	private def handleSubsNotifications(in:DataInputStream ) = {
		val substID=in.readInt
		val subs=subscriptionMap.get(substID) 
		
		subs match {
			case subscriber:SimpleSubscriber => { val nt=NotificationType(in.readInt)
				//print("simple "+nt)
				nt match {
					case NotificationType.FieldChanged => {
						val inst=InstanceData.readWithChildInfo(Reference(in),in)
						//System.out.println(" field changed:"+inst)
						runInPool(subscriber.func(NotificationType.FieldChanged,IndexedSeq(inst)))
					}
					case NotificationType.childAdded => {
						//val atPos=in.readInt						
						val inst=InstanceData.readWithChildInfo(Reference(in),in)
						//System.out.println(" child added:"+inst)
						runInPool(subscriber.func(NotificationType.childAdded,IndexedSeq(inst)))
					}
					case NotificationType.instanceRemoved => {
						val ref=Reference(in)
						runInPool(subscriber.func(NotificationType.instanceRemoved,
							IndexedSeq(new InstanceData(ref,IndexedSeq(),Array(),Seq.empty,false)))) // empty instance
					}
					case NotificationType.sendData => {
						val list=readList(in)
						//System.out.println(" send Data:"+list)
						runInPool(subscriber.func(NotificationType.sendData,list))
					}
				}
			}
			/*case subscriber:PositionSubscriber => NotificationType(in.readInt) match {
				case NotificationType.FieldChanged => {
					val inst=InstanceData.readWithChildInfo(Reference(in),in)
					runInPool(subscriber.func(NotificationType.FieldChanged,IndexedSeq(inst),0))
				}
				case NotificationType.childAdded => {
					//val atPos=in.readInt
					val inst=InstanceData.readWithChildInfo(Reference(in),in)
					runInPool(subscriber.func(NotificationType.childAdded,IndexedSeq(inst),atPos))
				}
				case NotificationType.instanceRemoved => {
					val ref=Reference(in)
					runInPool(subscriber.func(NotificationType.instanceRemoved,
						IndexedSeq(new InstanceData(ref,Array(),Array(),false)),0)) // empty instance
				}
				case NotificationType.sendData => {
					val list=readList(in)
					runInPool(subscriber.func(NotificationType.sendData,list,0))
				}
			}*/
			case factSubs:FactSubscriber[_] => {
				val nt=NotificationType(in.readInt)
				//print("fact ":+nt)
				nt match {
					case NotificationType.FieldChanged => {						
						val inst=factSubs.factory.createObject(Reference(in),in)
						//System.out.println(" field changed:"+inst)
						runInPool(factSubs.func(NotificationType.FieldChanged,IndexedSeq(inst)))
					}
					case NotificationType.childAdded => {
						
						//val atPos=in.readInt
						val inst=factSubs.factory.createObject(Reference(in),in)
						//System.out.println(" child added :"+inst)
						runInPool(factSubs.func(NotificationType.childAdded,IndexedSeq(inst)))
					}
					case NotificationType.instanceRemoved => {
						val ref=Reference(in)
						runInPool(factSubs.func(NotificationType.instanceRemoved,
							IndexedSeq(factSubs.factory.createEmptyObject(ref)))) // empty instance
					}
					case NotificationType.sendData => {						
						val list=readListWithFactory(in,factSubs.factory)
						//System.out.println(" send Data:"+list)
						runInPool(factSubs.func(NotificationType.sendData,list))
					}
				}
			}	
		}				
	}	
	
	private def handleCommandResponse(in:DataInputStream ) = {
		
		val hasError=in.readBoolean
		if(hasError) {
			val error=CommandError.read(in)
			commandResultQueue.put(None)
			printErrorMessage( error.getMessage)
		}
		else {
			val result:Option[Constant]= if(in.readBoolean) Some(Expression.readConstant(in))
																 else None
      printErrorMessage(" ")																 
		  //System.out.println("command Response "+result+" "+Thread.currentThread)
		
		  commandResultQueue.put(result)
		}
	}
	
	def askEnquiry(in:DataInputStream ) = {
		ParamQuestion.fromXML(scala.xml.XML.loadString (in.readUTF)) match {
			case Some(question)=> DialogManager.startEnquiryDialog(question)
			case _ => 
		}
		
		
	}
	
	def answerEnquiry(params:Seq[(String,Constant)])= {
		sock.sendData(ClientCommands.answerEnquiry) { out =>
			out.writeInt(params.size)
			params foreach((x) => {out.writeUTF(x._1); x._2 .write(out)})
		}
	}
	
	
	def registerSetupListener(listener:Function0[Unit]) {
		if(isSetup) listener() // call listener instantly when types are already setup
		else setupListenerMap+=listener // else put it in the list to be notified later
	}
	
	def registerAfterListener(listener:Function0[Unit]) {
		if(isSetup) listener() // call listener instantly when types are already setup
		else afterSetupListeners+=listener // else put it in the list to be notified later
	}
	
	
	def registerStoreSettingsListener(listener:Function0[Unit]) {
		storeSettingsListenerMap+=listener
		
	}
	
	def notifySetupListeners() = {
		setupListenerMap.foreach(a => a())
		isSetup=true
	}
	
	def notifyAfterSetupListeners() = runInPool{
		afterSetupListeners.foreach(a => a())
		
	}
	
	
	
	def notifyStoreSettingsListeners() = {
		storeSettingsListenerMap.foreach(a => a())
	}
	
	
	/** runs a given function in a new Thread from the ThreadPool
	 *  to avoid deadlocks
	 */
	def runInPool( a: => Unit) = {
		myPool.execute(new Runnable() {
						            	def run = a})
	}
	
	def runSw (func: =>Unit) = 
		SwingUtilities.invokeLater(new Runnable { def run =
			func
	})
		
	// ************************************ UNDO ********************************************
	
	private def lockForUndo(in:DataInputStream ) = {
		val lockUser=in.readUTF
		System.out.println("LOCK FOR UNDO by User "+lockUser)
	}
	
	private def releaseUndoLock(in:DataInputStream ) = {
		System.out.println("UNDO LOCK RELESED")
	}
	
	private def sendUndoInformation(in:DataInputStream ) = {		
		val stepListSize=in.readInt
		//System.out.println("Sending Undo Information "+stepListSize)
		val stepList=collection.mutable.ArrayBuffer[TransStepData]()
		for(i <-0 until stepListSize) {
			val newElem=TransStepData.read(in) 
			stepList+= newElem
			//System.out.println(newElem.toString)
		}		
		if(stepListReader!=null) stepListReader.loadStepList(stepList)
		//System.out.println("send ready ")
			
	}
	
	def registerStepListReader(slr:StepListReader)= {
		stepListReader=slr
	}
	
	def requestUndoData() = {
		sock.sendData(ClientCommands.requestUndoData) { out =>
		  
		}
	}
	
	def doUndo() = {
		sock.sendData(ClientCommands.undo) { out =>
		  
		}
	}
	
	def stopUndo() = {
		sock.sendData(ClientCommands.stopUndo) { out =>
		  
		}
	}
	
	def registerErrorListener(newListener:ErrorListener) = errorListeners += newListener
	
	def printErrorMessage(message:String) = errorListeners foreach (_.printError( message))
	
	
}