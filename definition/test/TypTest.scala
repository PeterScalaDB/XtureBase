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
	val rudolfClass=new ServerObjectClass("Rudolf",3,"Rudolfs Klasse",IndexedSeq(new FieldDefinition("GewerkName",DataType.StringTyp ),
		  new FieldDefinition("GewerkSumme",DataType.DoubleTyp )),IndexedSeq.empty,IndexedSeq(PropertyFieldDefinition("Kinder",false,0)),
		  IndexedSeq.empty,IndexedSeq.empty,Seq.empty,"",NOFORMAT,NOFORMAT,NOFORMAT)
	 
	
	
	
	val hugoClass=new ServerObjectClass("Hugo",7,"hugos klasse",IndexedSeq(new FieldDefinition("Hugofeld",DataType.StringTyp ),new FieldDefinition("HugoWert",
  		DataType.DoubleTyp )),IndexedSeq.empty,IndexedSeq(PropertyFieldDefinition("Subelements",false,0)),IndexedSeq.empty,IndexedSeq.empty,
  		IndexedSeq(3 ),"",NOFORMAT,NOFORMAT,NOFORMAT)
	  
  
  @Test def FieldDefinitionTest()
  {
  	val x=new FieldDefinition("GewerkName",DataType.StringTyp)
  	assertEquals(FieldDefinition.fromXML(x.toXML),x)  	
  }
  
  
  
  @Test def ClassTest()
  {
  	System.out.println(rudolfClass.toXML);
  	System.out.println(hugoClass.toXML)
  	System.out.println(".................................................")
  	val baclass=ServerObjectClass.fromXML(rudolfClass.toXML)
  	assertEquals(baclass.name,rudolfClass.name)
  	assertEquals(baclass.description,rudolfClass.description)
  	//Console.System.out.println(baclass.toXML)
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
  	System.out.println(AllClasses.get.asInstanceOf[ServerClassList].toXML())
  }
  
  @Test def pathTest()
  {
  	System.out.println(FSPaths.configDir )
  }
  
  
  
  
 
}


