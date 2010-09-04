/**
 * Author: Peter Started:04.09.2010
 */
package client.comm

import definition.data._
import definition.typ._
import definition.comm._
import java.util.concurrent._
import java.io._

/** manages Queries and Subscriptions
 * 
 */
object ClientQueryManager {
	//type InstArrayFunc=(Array[InstanceData])=> Unit
	type UpdateFunc=(Int,NotificationType.Value,Array[InstanceData])=>Unit
	
	//case class Query(func: InstArrayFunc)		
	case class Subscriber(func: UpdateFunc)
	
	private val queryQueue = new SynchronousQueue[Array[InstanceData]]()	
	private val newSubscriberQueue=new ArrayBlockingQueue[Subscriber](3)
	private val subscriptionMap=new ConcurrentHashMap[Int,Subscriber]()
	private val commandResultQueue = new SynchronousQueue()
	
	private var sock:ClientSocket=null
	
	def setClientSocket(newSock:ClientSocket) = {
		sock=newSock
		// register Handler
		sock.registerCommandHandler(ServerCommands.sendQueryResponse)(handleQueryResults)
		sock.registerCommandHandler(ServerCommands.acceptSubscription)(handleAcceptSubscription)
		sock.registerCommandHandler(ServerCommands.sendSubscriptionNotification )(handleSubsNotifications)
	}
	
	// helper func
	private def readArray(in:DataInputStream):Array[InstanceData] = {
			val numData=in.readInt
			val retArray=new Array[InstanceData](numData)
					for(i <- 0 until numData) retArray(i)=InstanceData.read(Reference(in), in)
			retArray
		}
	
	private def handleQueryResults(in:DataInputStream) = 	{
		val data=readArray(in)
		println("Handling Query result data size:"+data.size)
		queryQueue.put(data)
		
	}
	
	
	
	def queryInstance(ref:Reference,propertyField:Byte):Array[InstanceData] = 	{		
		sock.sendData(ClientCommands.queryInstance ) {out =>			
			println("Sending Query request "+ref)
			ref.write(out)
			out.writeByte(propertyField)
		}
		queryQueue.take
	}
	
	
	
	def createSubscription(parentRef:Reference,propertyField:Byte)(updateFunc: UpdateFunc) = {		
		
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( Subscriber(updateFunc))
			println("adding subscription "+parentRef)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
	}
	
	private def handleAcceptSubscription(in:DataInputStream) = {
		val subsID:Int=in.readInt
		val data=readArray(in)
		println("Handling accept subs "+data)
		val subs:Subscriber=newSubscriberQueue.take()			
		subscriptionMap.put(subsID,subs)
		subs.func(subsID,NotificationType.sendData,data)
	}
	
	private def handleSubsNotifications(in:DataInputStream ) = {
		val substID=in.readInt
		val subscriber=subscriptionMap.get(substID) 
		
		NotificationType(in.readInt) match {
			case NotificationType.FieldChanged => {
				val inst=InstanceData.read(Reference(in),in)
				subscriber.func(substID,NotificationType.FieldChanged,Array(inst))
			}
		}
		
	}
	
	def removeSubscription(subsID:Int) = {
		sock.sendData(ClientCommands.stopSubscription ) {out =>
			out.writeInt(subsID)			
		}
		subscriptionMap.remove(subsID)
	}
}