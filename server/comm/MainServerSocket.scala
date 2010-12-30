/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import java.net._
import java.io._
import server.config._


/**
 * 
 */
object MainServerSocket extends Thread("MainSock") {
  override def run():Unit = {
  	try {
      val listener = new ServerSocket(FSPaths.serverPort);
      System.out.println("ServerSocket ready on port "+FSPaths.serverPort)
      while (true)
        new UserSocket(listener.accept()).start();
       
  	}
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: "+FSPaths.serverPort);
        System.exit(-1)
    }
  		
  }
}