/**
 * Author: Peter Started:29.08.2010
 */
package client.comm

import definition.comm._
import java.net._
import java.io._
import definition.typ.AllClasses
import definition.data._
import definition.expression._
import scala.actors._
import scala.collection.mutable.HashMap




/** manages the communication with the server
 * 
 */
class ClientSocket(serverAddress: InetAddress,port:Int,name:String,password:String) extends Thread("Socket-Thread") {
	
	private val socket = new Socket(serverAddress, port)
	val out =  new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()))
	private val outStreamLock : AnyRef = new Object()
	var wantQuit=false
	
	
	type CommandHandlerFunc= (DataInputStream)=>Unit
	
	
	private val commandHandlerMap=new scala.collection.mutable.HashMap[ServerCommands.Value,CommandHandlerFunc]()
	
	
	override def run():Unit = {
		 try {			
			 val in = new DataInputStream(new BufferedInputStream(socket.getInputStream()))
			 // send user name and password
			 writeOut(name)
			 writeOut(password)
			 Thread.`yield`()
			 val retValue=in.readUTF()
			 if(retValue !="welcome" ) {println("not welcome, message: "+retValue); return }
			 println("Logged in to "+serverAddress)
			 sendData(ClientCommands.getTypes ){out =>}
			 handleCommands(in)
		 }
		 catch {
      case e: IOException =>
        e.printStackTrace()
     }
	}
	
	private def writeOut(st:String ) = {out.writeUTF(st);out.flush()}
	
	private def handleCommands(in:DataInputStream):Unit = {	
		
		while(true)
		{				
			try {
				val command =ServerCommands(in.readByte.toInt)
				println("ServerCommand: "+command)
				command match {
					case ServerCommands.sendTypes  => readInTypes(in)
					case ServerCommands.wantQuit => {quitApplication();return }					
					case a => if (commandHandlerMap.contains(a)) 
						              commandHandlerMap(a)(in)						             
										else println("ServerCommand "+a+" not handled")					
				}
			}
			catch {
				case a:EOFException => if(!wantQuit) a.printStackTrace else return
				case e:Exception => e.printStackTrace; return
			}
		}			
	}
		
	private def readInTypes(in: DataInputStream) = {
		val xmlString=in.readUTF()
		//println(xmlString)
		AllClasses.fromXML(scala.xml.XML.loadString(xmlString))
		//println(AllClasses.toXML)
	}
	
	
	
	
	
	// ************************************ COMMANDS ***************************************
	
	def registerCommandHandler(command:ServerCommands.Value)(func: (DataInputStream)=>Unit) = {
		commandHandlerMap.put(command,func)
	}
	
		
	
	def quitApplication() = 	{
		// Shutdown all data
		// store user settings
		
		// logout
		println("Logging out")
		wantQuit=true
		sendData(ClientCommands.logOut ) {out =>}
		System.exit(0)
	}
		
	def sendData(command:ClientCommands.Value)(func:(DataOutputStream) => Unit) = 	{
		try {
			outStreamLock.synchronized {
				out.writeByte(command.id.toByte)
				func(out)
				out.flush()
			}
		}
		catch {			
			case e: IOException =>
			e.printStackTrace();
		}
	}	
	
}

object ClientSocket {
	def main(args: Array[String]): Unit = { 
		
		if(args.length<4 ) { println("Usage: ClientSocket host portnr"); return }
  	val sock=new ClientSocket(InetAddress.getByName(args(0)),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()
  	println("Query Database ")
  	
  	println("quick:" + ClientQueryManager.queryInstance(Reference(3,1), -1).mkString(","))
  	var substID=0
  	substID=ClientQueryManager.createSubscription(Reference(3,3),0) { 
  		(notification:NotificationType.Value,data:Array[InstanceData])=>
  		println("Get data id:"+substID+ " Type:"+notification)
  		
  		
  		println("Num:"+data.size)
  		println("quick inside:" + ClientQueryManager.queryInstance(data(0).ref, -1).mkString(","))
  		for(elem <-data ) {
  			println(elem)
  			ClientQueryManager.createSubscription(elem.ref,0){ 
  				(notification:NotificationType.Value,data:Array[InstanceData]) =>
  				println("Sub 1st:"+data.size)  		
  				
  				println("quick inside:" + ClientQueryManager.queryInstance(Reference(3,2), -1).mkString(","))
  			}
  		}
  		
  	}
  	
  	println("Finish")  	
  	readLine()
  	ClientQueryManager.writeInstanceField(Reference(3,4),0,StringConstant("a new Value "+Math.random*100))
  	readLine()
  	ClientQueryManager.removeSubscription(substID)
  	sock.quitApplication()
  	
  }
}
