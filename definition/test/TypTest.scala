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


/**
 * 
 */
class TypTest extends JUnitSuite {
	val rudolfClass=new ObjectClass("Rudolf",3,"Rudolfs Klasse",IndexedSeq(FieldDefinition("GewerkName",DataType.StringTyp ),
		  FieldDefinition("GewerkSumme",DataType.DoubleTyp )),IndexedSeq(PropertyFieldDefinition("Kinder",false,0)),Seq.empty)
	 
	
	
	val hugoClass=new ObjectClass("Hugo",7,"hugos klasse",IndexedSeq(FieldDefinition("Hugofeld",DataType.StringTyp ),FieldDefinition("HugoWert",
  		DataType.DoubleTyp )),IndexedSeq(PropertyFieldDefinition("Subelements",false,0)),IndexedSeq("Rudolf" ))
	  
  
  @Test def FieldDefinitionTest()
  {
  	val x=FieldDefinition("GewerkName",DataType.StringTyp)
  	assertEquals(FieldDefinition.fromXML(x.toXML),x)  	
  }
  
  
  
  @Test def ClassTest()
  {
  	//Console.println(rudolfClass.toXML);
  	val baclass=ObjectClass.fromXML(rudolfClass.toXML)
  	assertEquals(baclass.name,rudolfClass.name)
  	assertEquals(baclass.description,rudolfClass.description)
  	//Console.println(baclass.toXML)
  	//assertEquals(baclass.versions,dieclass.versions)
  }
  
  @Test def AllClassesTest()
  {
  	AllClasses.addClass(rudolfClass)
  	AllClasses.addClass(hugoClass)
  	AllClasses.resolveFields
  	val file=FSPaths.configDir+"types.xml"
  	scala.xml.XML.save(file,AllClasses.toXML,"UTF-8",true,null)
  	AllClasses.fromXML( xml.XML.loadFile(file))
  	Console.println(AllClasses.toXML)
  }
  
  @Test def pathTest()
  {
  	Console.println(FSPaths.configDir )
  }
  
  
  
  
 
}


