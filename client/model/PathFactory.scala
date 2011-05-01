/**
 * Author: Peter Started:10.11.2010
 */
package client.model

import definition.data._
import client.comm.{ClientQueryManager,UserSettings}
import collection.immutable._
/** saves the last used paths
 * 
 */



object PathFactory {
	
	class Entry (var used:Boolean,var list:Seq[Reference])
	
  val pathList= collection.mutable.ArrayBuffer[Entry]()
  var standardRef:Reference=null
  
  def loadSettings=  {
  	val num=UserSettings.getIntProperty("TablePaths","num",0)
  	for(i <- 0 until num)
  		pathList += new Entry(false,UserSettings.getListProperty[Reference]("TablePaths",i.toString,collection.immutable.Seq.empty))
  	val sr=UserSettings.getStringProperty("TablePath","standard","(110,1)")
  	val sr1=sr.substring(1,sr.length-1).split(',')
  	standardRef=new Reference(sr1(0).toInt,sr1(1).toInt)
  }
	
	
	ClientQueryManager.registerSetupListener(()=> {
		loadSettings
	})
	
  
  ClientQueryManager.registerStoreSettingsListener (() => {
  	//System.out.println("store path info "+pathList.mkString)
  	UserSettings.setIntProperty("TablePaths","num",pathList.size)
  	for(i <-pathList.indices)
  		UserSettings.setListProperty[Reference]("TablePaths",i.toString,pathList(i).list)
  }	)
  
  def getNextPathEntry:(Int,Seq[Reference]) = {
		
  	for( i <-pathList.indices) 
  		if(! pathList(i).used) {
  			pathList(i).used=true
  			System.out.println("get Path Entry:"+i+" "+pathList(i).list .mkString(","))
  			return (i,pathList(i).list)
  		}
  	// no free entry found, create a new one:
  	val newEntry=new Entry(true,List(standardRef))
  	pathList +=newEntry
  	System.out.println("create next Path Entry:"+(pathList.size-1)+" "+newEntry.list .mkString(","))
  	(pathList.size-1,newEntry.list)
  }
  
  def releasePathEntry(index:Int,path:Seq[Reference])= {
  	System.out.println("realease Entry "+index+" "+path.mkString)
  	val entry=pathList(index)
  	entry.used=false
  	entry.list=path
  }
  
}