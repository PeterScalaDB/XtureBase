/**
 * Author: Peter Started:06.09.2010
 */
package server.test

/**
 * 
 */
object SimpleProfiler {
  var startTime:Long=0
  var lastTime:Long=0
  var dprint=false
  
  
  def startMeasure(label:String) = if(dprint){
  	 print("["+label+" ")
  	startTime=System.currentTimeMillis();
  	lastTime=startTime
  }
  
  def measure(label:String) = if(dprint) {
  	val between=System.currentTimeMillis();
  	 print(label+":"+(between-lastTime)+" ")
  	lastTime=between
  }
  
  def finish(label:String) = if(dprint) {
  	measure(label)
  	println("= "+(lastTime-startTime))
  }
  	
  
  
}