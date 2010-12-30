/**
 * Author: Peter Started:13.12.2010
 */
package client.dialog
import java.io._
import client.comm.ErrorListener
/**
 * 
 */



object LogOutputStream extends FilterOutputStream (new ByteArrayOutputStream()){
	
	val listeners=collection.mutable.HashSet[ErrorListener]()

	def registerListener(newListener:ErrorListener)= listeners+=newListener

	override def write( b:Array[Byte]) = {
		val aString = new String(b);
		//System.out.println("writeb "+aString)
		if(aString.length>0 && aString!="\n")
		for(l <-listeners)
			l.printError(aString);
	}

	override def write(b:Array[Byte], off:Int, len:Int) = {		
		val aString = new String(b , off , len).trim;		
		if(aString.length>2 && aString!="\n") {
			//System.out.println("|"+aString.replace('\n', ' ').replace('\r',' '))
			for(l <-listeners)
				l.printError(aString+"\n");
		}
		         
	}
	
	val ps=new PrintStream(this,true)
	//java.lang.System.setErr(ps)

}

