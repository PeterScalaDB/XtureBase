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

/** manages Queries and Subscriptions
 * 
 */
object ClientQueryManager {
	//type InstArrayFunc=(Array[InstanceData])=> Unit
	type UpdateFunc=(NotificationType.Value,IndexedSeq[InstanceData])=>Unit
	
	type FactUpdateFunc[T<: Referencable]=(NotificationType.Value,IndexedSeq[T])=>Unit
	
	//type PathUpdateFunc=(PathNotificationType.Value,Array[InstanceData])=>Unit
	
	//case class Query(func: InstArrayFunc)
	trait Subscriber
	case class SimpleSubscriber(func: UpdateFunc) extends Subscriber
	case class FactSubscriber[T<: Referencable](factory:SubscriptionFactory[T],func: FactUpdateFunc[T]) extends Subscriber
	//case class PathSubscriber(func:PathUpdateFunc)
	
	private val queryQueue = new SynchronousQueue[IndexedSeq[InstanceData]]()	
	private val newSubscriberQueue=new ArrayBlockingQueue[Subscriber](3)
	private val subscriptionMap=new ConcurrentHashMap[Int,Subscriber]()
	
	
	
	private val commandResultQueue = new SynchronousQueue[Option[Constant]]()
	private val subscriptionAcceptQueue = new SynchronousQueue[Int]()
	//private val pathSubsAcceptQueue=new SynchronousQueue[Int]()
	
	private var sock:ClientSocket=null
	private val myPool=Executors.newCachedThreadPool() 
	private var isSetup=false
	private var setupListenerMap=collection.mutable.HashSet[Function0[Unit]]()
	private val storeSettingsListenerMap= collection.mutable.HashSet[Function0[Unit]]()
	
	def setClientSocket(newSock:ClientSocket) = {
		sock=newSock
		// register Handler
		sock.registerCommandHandler(ServerCommands.sendQueryResponse)(handleQueryResults)
		sock.registerCommandHandler(ServerCommands.acceptSubscription)(handleAcceptSubscription)	
		sock.registerCommandHandler(ServerCommands.sendSubscriptionNotification )(handleSubsNotifications)
		sock.registerCommandHandler(ServerCommands.sendCommandResponse )(handleCommandResponse)
	}
	
		
		
	def queryInstance(ref:Reference,propertyField:Byte):IndexedSeq[InstanceData] = 	{		
		sock.sendData(ClientCommands.queryInstance ) {out =>			
			//println("Sending Query request "+ref + " "+Thread.currentThread)
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
			//println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}
	
	def createFactSubscription[T <:Referencable](parentRef:Reference,propertyField:Byte,factory:SubscriptionFactory[T])
	(updateFunc: FactUpdateFunc[T]):Int = {		
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( FactSubscriber(factory,updateFunc))
			//println("adding subscription "+parentRef+ " "+Thread.currentThread)
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
	
	def createPathSubscription(path:Seq[Reference])(updateFunc:UpdateFunc) = {		
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
	 * @param newPathPos the number of the element that should be the last one
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
		else println("ERROR: subscription "+subsID+" not found when removing")
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
	
	/** creates an instance and returns the instanceID
	 * @param classType the typeID of the new instance
	 * @param owners the owners of the new instance
	 * @return the ID of the new class
	 */
	def createInstance(classType:Int,owners:Array[OwnerReference]):Long = {
		sock.sendData(ClientCommands.createInstance ) { out =>
		   out.writeInt(classType)
		   out.writeInt(owners.size)
		   for(owner <-owners)
		  	 owner.write(out)			
		}
		//println("create "+Thread.currentThread.getName)
		commandResultQueue.take() match {
			case Some(const) => const.toLong
			case None => throw new IllegalArgumentException("no instance ID returned when creating type "+classType)
		}
	}
	
	def copyInstance(ref:Reference,fromOwner:OwnerReference,toOwner:OwnerReference):Long = {
		sock.sendData(ClientCommands.copyInstance ) { out =>
			ref.write(out)
			fromOwner.write(out)
			toOwner.write(out)
		}
		commandResultQueue.take() match {
			case Some(const) =>const.toLong
			case None => throw new IllegalArgumentException("no instance ID returned when copying instance "+ref)
		}
	}
	
	
	def deleteInstance(ref:Reference) = {
		sock.sendData(ClientCommands.deleteInstance ) { out =>
			ref.write(out)
		}
		commandResultQueue.take() 
	}
	
	def executeAction(instList:Seq[Referencable],actionName:String,params:Seq[(String,Constant)])= {
		sock.sendData(ClientCommands.executeAction) { out =>
			out.writeInt(instList.size)
			instList foreach(_.ref.write(out))
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
		//println("Handling Query result data size:"+data.size+ " "+Thread.currentThread)
		
		queryQueue.put(data)
		
	}
	
	
	private def handleAcceptSubscription(in:DataInputStream) = {
		val subsID:Int=in.readInt		
		//println("Handling accept subs subsID:"+subsID)
		
		val subs:Subscriber=newSubscriberQueue.take()			
		subscriptionMap.put(subsID,subs)
		subscriptionAcceptQueue.put(subsID)
		subs match {			
			case a:SimpleSubscriber =>{
				val data=readList(in)
				a.func(NotificationType.sendData,data)
			}
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
			case subscriber:SimpleSubscriber => NotificationType(in.readInt) match {
				case NotificationType.FieldChanged => {
					val inst=InstanceData.readWithChildInfo(Reference(in),in)
					runInPool(subscriber.func(NotificationType.FieldChanged,IndexedSeq(inst)))
				}
				case NotificationType.childAdded => {
					val inst=InstanceData.readWithChildInfo(Reference(in),in)
					runInPool(subscriber.func(NotificationType.childAdded,IndexedSeq(inst)))
				}
				case NotificationType.instanceRemoved => {
					val ref=Reference(in)
					runInPool(subscriber.func(NotificationType.instanceRemoved,
						IndexedSeq(new InstanceData(ref,Array(),Array(),false)))) // empty instance
				}
				case NotificationType.sendData => {
					val list=readList(in)
					runInPool(subscriber.func(NotificationType.sendData,list))
				}
			}
			case factSubs:FactSubscriber[_] => NotificationType(in.readInt) match {
				case NotificationType.FieldChanged => {
					val inst=factSubs.factory.createObject(Reference(in),in)
					runInPool(factSubs.func(NotificationType.FieldChanged,IndexedSeq(inst)))
				}
				case NotificationType.childAdded => {
					val inst=factSubs.factory.createObject(Reference(in),in)
					runInPool(factSubs.func(NotificationType.childAdded,IndexedSeq(inst)))
				}
				case NotificationType.instanceRemoved => {
					val ref=Reference(in)
					runInPool(factSubs.func(NotificationType.instanceRemoved,
						IndexedSeq(factSubs.factory.createEmptyObject(ref)))) // empty instance
				}
				case NotificationType.sendData => {
					val list=readListWithFactory(in,factSubs.factory)
					runInPool(factSubs.func(NotificationType.sendData,list))
				}
			}
		}
				
	}
	
	
	
	
	private def handleCommandResponse(in:DataInputStream ) = {
		
		val hasError=in.readBoolean
		if(hasError) {
			val error=CommandError.read(in)
			commandResultQueue.put(None)
			println( error)
		}
		else {
			val result:Option[Constant]= if(in.readBoolean) Some(Expression.readConstant(in))
																 else None
		  //println("command Response "+result+" "+Thread.currentThread)
		
		  commandResultQueue.put(result)
		}
	}
	
	def registerSetupListener(listener:Function0[Unit]) {
		if(isSetup) listener() // call listener instantly when types are already setup
		else setupListenerMap+=listener // else put it in the list to be notified later
	}
	
	def registerStoreSettingsListener(listener:Function0[Unit]) {
		storeSettingsListenerMap+=listener
		
	}
	
	def notifySetupListeners() = runInPool{
		setupListenerMap.foreach(a => a())
		isSetup=true
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
		
	
	
}