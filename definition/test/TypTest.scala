/**
 * Author: Peter Started:26.06.2010
 */
package definition.test

import org.scalatest.junit.JUnitSuite

import org.junit.Assert._
import definition.typ._
import org.junit.Test
import org.junit.Before
import server.config._
import server.storage._


/**
 * 
 */
class TypTest extends JUnitSuite {
	val rudolfClass=new ServerObjectClass("Rudolf",3,"Rudolfs Klasse",IndexedSeq(FieldDefinition("GewerkName",DataType.StringTyp ),
		  FieldDefinition("GewerkSumme",DataType.DoubleTyp )),IndexedSeq.empty,IndexedSeq(PropertyFieldDefinition("Kinder",false,0)),
		  IndexedSeq.empty,IndexedSeq.empty,Seq.empty,"",NOFORMAT,NOFORMAT,NOFORMAT)
	 
	
	
	
	val hugoClass=new ServerObjectClass("Hugo",7,"hugos klasse",IndexedSeq(FieldDefinition("Hugofeld",DataType.StringTyp ),FieldDefinition("HugoWert",
  		DataType.DoubleTyp )),IndexedSeq.empty,IndexedSeq(PropertyFieldDefinition("Subelements",false,0)),IndexedSeq.empty,IndexedSeq.empty,
  		IndexedSeq("Rudolf" ),"",NOFORMAT,NOFORMAT,NOFORMAT)
	  
  
  @Test def FieldDefinitionTest()
  {
  	val x=FieldDefinition("GewerkName",DataType.StringTyp)
  	assertEquals(FieldDefinition.fromXML(x.toXML),x)  	
  }
  
  
  
  @Test def ClassTest()
  {
  	Console.println(rudolfClass.toXML);
  	println(hugoClass.toXML)
  	println(".................................................")
  	val baclass=ServerObjectClass.fromXML(rudolfClass.toXML)
  	assertEquals(baclass.name,rudolfClass.name)
  	assertEquals(baclass.description,rudolfClass.description)
  	//Console.println(baclass.toXML)
  	//assertEquals(baclass.versions,dieclass.versions)
  }
  
  @Test def AllClassesTest()
  {
  	val sc=new ServerClassList(null)
  	AllClasses.set(sc)
  	sc.addClass(rudolfClass)
  	sc.addClass(hugoClass)
  	AllClasses.get.resolveFields
  	val file=FSPaths.configDir+"testtypes.xml"
  	scala.xml.XML.save(file,AllClasses.get.asInstanceOf[ServerClassList].toXML(),"UTF-8",true,null)
  	AllClasses.set(new ServerClassList( xml.XML.loadFile(file)))
  	Console.println(AllClasses.get.asInstanceOf[ServerClassList].toXML())
  }
  
  @Test def pathTest()
  {
  	Console.println(FSPaths.configDir )
  }
  
  
  
  
 
}


