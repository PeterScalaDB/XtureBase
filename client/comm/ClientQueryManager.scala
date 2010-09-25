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

/** manages Queries and Subscriptions
 * 
 */
object ClientQueryManager {
	//type InstArrayFunc=(Array[InstanceData])=> Unit
	type UpdateFunc=(NotificationType.Value,IndexedSeq[InstanceData])=>Unit
	
	//type PathUpdateFunc=(PathNotificationType.Value,Array[InstanceData])=>Unit
	
	//case class Query(func: InstArrayFunc)		
	case class Subscriber(func: UpdateFunc)
	//case class PathSubscriber(func:PathUpdateFunc)
	
	private val queryQueue = new SynchronousQueue[IndexedSeq[InstanceData]]()	
	private val newSubscriberQueue=new ArrayBlockingQueue[Subscriber](3)
	private val subscriptionMap=new ConcurrentHashMap[Int,Subscriber]()
	//private val newPathSubscriberQueue=new ArrayBlockingQueue[PathSubscriber](3)
	//private val pathSubscriptionMap=new ConcurrentHashMap[Int,PathSubscriber]()
	
	
	private val commandResultQueue = new SynchronousQueue[Option[Constant]]()
	private val subscriptionAcceptQueue = new SynchronousQueue[Int]()
	//private val pathSubsAcceptQueue=new SynchronousQueue[Int]()
	
	private var sock:ClientSocket=null
	private val myPool=Executors.newCachedThreadPool() 
	
	def setClientSocket(newSock:ClientSocket) = {
		sock=newSock
		// register Handler
		sock.registerCommandHandler(ServerCommands.sendQueryResponse)(handleQueryResults)
		sock.registerCommandHandler(ServerCommands.acceptSubscription)(handleAcceptSubscription)
		//sock.registerCommandHandler(ServerCommands.acceptPathSubscription)(handleAcceptPathSubscription)
		sock.registerCommandHandler(ServerCommands.sendSubscriptionNotification )(handleSubsNotifications)
		//sock.registerCommandHandler(ServerCommands.sendPathSubsNotification )(handlePathSubsNotifications)
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
			newSubscriberQueue.add( Subscriber(updateFunc))
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
	
	def createPathSubscription(path:IndexedSeq[Reference])(updateFunc:UpdateFunc) = {
		sock.sendData(ClientCommands.startPathSubscription ) {out =>
			newSubscriberQueue.add( Subscriber(updateFunc))
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
	
	def pathSubs_changePath(subsID:Int,newPath:IndexedSeq[Reference]) = {
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
	
	
	// ************************************* Internal routines *************************************
	
	private def readList(in:DataInputStream):IndexedSeq[InstanceData] = {
			val numData=in.readInt
			for(i <- 0 until numData) yield InstanceData.readWithChildInfo(Reference(in), in)
			
		}
	
	
	private def handleQueryResults(in:DataInputStream) = 	{
		val data=readList(in)
		//println("Handling Query result data size:"+data.size+ " "+Thread.currentThread)
		
		queryQueue.put(data)
		
	}
	
	
	private def handleAcceptSubscription(in:DataInputStream) = {
		val subsID:Int=in.readInt
		val data=readList(in)
		//println("Handling accept subs subsID:"+subsID)
		
		val subs:Subscriber=newSubscriberQueue.take()			
		subscriptionMap.put(subsID,subs)
		subscriptionAcceptQueue.put(subsID)
		runInPool(subs.func(NotificationType.sendData,data))
	}
	
	/*private def handleAcceptPathSubscription(in:DataInputStream) = {
		val subsID:Int=in.readInt
		val pathData=readArray(in)
		val listData=readArray(in)		
		
		val subs:PathSubscriber=newPathSubscriberQueue.take()			
		pathSubscriptionMap.put(subsID,subs)
		pathSubsAcceptQueue.put(subsID)
		runInPool(subs.func(PathNotificationType.sendPathList,pathData))
		runInPool(subs.func(PathNotificationType.sendChildList,listData))
	}*/
	
	private def handleSubsNotifications(in:DataInputStream ) = {
		val substID=in.readInt
		val subscriber=subscriptionMap.get(substID) 
		
		NotificationType(in.readInt) match {
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
	}
	
	/*private def handlePathSubsNotifications(in:DataInputStream ) = {
		val substID=in.readInt
		val subscriber=pathSubscriptionMap.get(substID) 
		
		PathNotificationType(in.readInt) match {
			case a @ PathNotificationType.sendChildList => {				
				runInPool(subscriber.func(a,readArray(in)))
			}
			case b @ PathNotificationType.sendPathList => {				
				runInPool(subscriber.func(b,readArray(in)))
			}			
			case c @ PathNotificationType.childAdded => {
				val inst=InstanceData.read(Reference(in),in)
				runInPool(subscriber.func(c,Array(inst)))
			}			
			case d @ PathNotificationType.childFieldChanged => {
				val inst=InstanceData.read(Reference(in),in)
				runInPool(subscriber.func(d,Array(inst)))
			}			
			case e @ PathNotificationType.childRemoved => {
				val ref=Reference(in)
				runInPool(subscriber.func(e, Array(new InstanceData(ref,0,Array())))) // empty instance
			}			
			case f @ PathNotificationType.pathElementChanged => {
				val inst=InstanceData.read(Reference(in),in)
				runInPool(subscriber.func(f,Array(inst)))				
			}
			case g @ PathNotificationType.pathElementRemoved => {
				val ref=Reference(in)
				runInPool(subscriber.func(g, Array(new InstanceData(ref,0,Array())))) // empty instance				
			}			
		}		
	}*/
	
	
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
	
	/** runs a given function in a new Thread from the ThreadPool
	 *  to avoid deadlocks
	 */
	def runInPool( a: => Unit) = {
		myPool.execute(new Runnable() {
						            	def run = a})
	}
		
	
	
}