/**
 * Author: Peter Started:29.08.2010
 */
package client.comm

import definition.comm._
import java.net._
import java.io._
import definition.data._
import definition.expression._
import definition.typ.{AllClasses,ClientClasses,SystemSettings}
import scala.actors._
import scala.collection.mutable.HashMap
import client.print.PrintQuestionHandler




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
	
	private val genDataHandlerMap=new collection.mutable.HashMap[GeneratorType.Value,GenDataReceiver]()
	
	
	
	override def run():Unit = {
		 try {			
			 val in = new DataInputStream(new BufferedInputStream(socket.getInputStream()))
			 // send user name and password
			 writeOut(name)
			 writeOut(password)
			 Thread.`yield`()
			 val retValue=in.readUTF()
			 if(retValue !="welcome" ) {System.out.println("not welcome, message: "+retValue); return }
			 System.out.println("Logged in to "+serverAddress)
			 sendData(ClientCommands.getTypes ){out =>}			 
			 sendData(ClientCommands.getUserSettings ){out =>}
			 Thread.`yield`()
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
				System.out.println("ServerCommand: "+command)
				command match {
					case ServerCommands.sendTypes  => readInTypes(in)
					case ServerCommands.sendUserSettings => readUserSettings(in)
					case ServerCommands.wantQuit => {quitApplication();return }					
					case a => if (commandHandlerMap.contains(a)) 
						              commandHandlerMap(a)(in)						             
										else System.out.println("ServerCommand "+a+" not handled")					
				}
			}
			catch {
				case a:EOFException => if(!wantQuit) a.printStackTrace else return
				case e:Exception => e.printStackTrace 
			}
		}			
	}
		
	private def readInTypes(in: DataInputStream) = {
		val xmlString=in.readUTF()
		SystemSettings.settings=new ClientSystemSettings(in)
		//System.out.println(xmlString)
		AllClasses.set(new ClientClasses(scala.xml.XML.loadString(xmlString)))	
		//System.out.println(AllClasses.get.getClassByID(40).actions .mkString)
		//System.out.println(AllClasses.get.getClassByID(40).createActions .mkString)
		//System.out.println(AllClasses.get.classList.mkString)
		System.out.println("Types got")
	}
	
	private def readUserSettings(in:DataInputStream) = {
		val length=in.readInt
		print(length +" ")
		val buffer=new Array[Byte](length)
		if(length>0) in.read(buffer,0,length)
		val s= new String(buffer)
		UserSettings.parse(s)
		System.out.println("reading user settings "+UserSettings.writeProperties)
		
		
		ClientQueryManager.notifySetupListeners
		ClientQueryManager.notifyAfterSetupListeners
		
		// wrong place, but it works
		genDataHandlerMap(GeneratorType.printGenerator )=PrintQuestionHandler
	}
	
	
	
	
	
	
	
	
	// ************************************ COMMANDS ***************************************
	
	def registerCommandHandler(command:ServerCommands.Value)(func: (DataInputStream)=>Unit) = {
		commandHandlerMap.put(command,func)
	}
	
	def registerGenDataHandler(gType:GeneratorType.Value,handler:GenDataReceiver)= genDataHandlerMap	
	
	def quitApplication() = 	{
		// Shutdown all data
		// save changes in user settings
		ClientQueryManager.notifyStoreSettingsListeners()		
		// store user settings
		System.out.println("writing settings")
		sendData(ClientCommands.writeUserSettings ) {out =>
			val buffer =UserSettings.writeProperties.getBytes
			out.writeInt(buffer.length)
			out.write(buffer,0,buffer.length)
		}
		// logout
		System.out.println("Logging out")
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
	
	
	// ****************************** COMMAND HANDLER ****************************************+++++
	
	registerCommandHandler(ServerCommands.sendGeneratedData )(in => {
		genDataHandlerMap(GeneratorType(in.readInt)).receiveData(in)
	})
	
}

object ClientSocket {
	def main(args: Array[String]): Unit = { 
		
		if(args.length<4 ) { System.out.println("Usage: ClientSocket host portnr"); return }
  	val sock=new ClientSocket(InetAddress.getByName(args(0)),args(1).toInt,args(2),args(3))
  	sock.start()
  	ClientQueryManager.setClientSocket(sock)
  	Thread.`yield`()
  	System.out.println("Query Database ")
  	
  	System.out.println("quick:" + ClientQueryManager.queryInstance(Reference(3,1), -1).mkString(","))
  	var substID=0
  	substID=ClientQueryManager.createSubscription(Reference(3,3),0) { 
  		(notification:NotificationType.Value,data:IndexedSeq[InstanceData])=>
  		System.out.println("Get data id:"+substID+ " Type:"+notification)
  		
  		
  		System.out.println("Num:"+data.size)
  		System.out.println("quick inside:" + ClientQueryManager.queryInstance(data(0).ref, -1).mkString(","))
  		for(elem <-data ) {
  			System.out.println(elem)
  			ClientQueryManager.createSubscription(elem.ref,0){ 
  				(notification:NotificationType.Value,data:IndexedSeq[InstanceData]) =>
  				System.out.println("Sub 1st:"+data.size)  		
  				
  				System.out.println("quick inside:" + ClientQueryManager.queryInstance(Reference(3,2), -1).mkString(","))
  			}
  		}
  		
  	}
  	
  	System.out.println("Finish")  	
  	readLine()
  	ClientQueryManager.writeInstanceField(Reference(3,4),0,StringConstant("a new Value "+Math.random*100))
  	readLine()
  	ClientQueryManager.removeSubscription(substID)
  	sock.quitApplication()
  	
  }
}
