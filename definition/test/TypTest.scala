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
	val oc=new ClassVersion(13,Array(FieldDefinition("GewerkName",DataType.StringTyp ),FieldDefinition("GewerkSumme",
  		DataType.CurrencyTyp )),Array(PropertyFieldDefinition("Kinder",false,0)),Nil,3)
	val o2=new ClassVersion(8,Array(FieldDefinition("GewerkName",DataType.StringTyp ),FieldDefinition("GewerkSumme",
  		DataType.CurrencyTyp ),FieldDefinition("sonstwas",DataType.DateTyp ) ),Array(),List(("Hugo",5)),3)
	val rudolfClass=new ObjectClass("Rudolf",3,"Rudolfs Klasse",List(oc,o2))
	val h1=new ClassVersion(5,Array(FieldDefinition("Hugofeld",DataType.StringTyp ),FieldDefinition("HugoWert",
  		DataType.DoubleTyp )),Array(PropertyFieldDefinition("Subelements",false,0)),Nil,7)
	val hugoClass=new ObjectClass("Hugo",7,"hugos klasse", List(h1))
	  
  
  @Test def FieldDefinitionTest()
  {
  	val x=FieldDefinition("GewerkName",DataType.StringTyp)
  	assertEquals(FieldDefinition.fromXML(x.toXML),x)  	
  }
  
  @Test def ClassVersionText()
  {  	
  	val ox=oc.toXML  	
  	val oxx=ClassVersion.fromXML(ox,4)  	
  	assertEquals(oxx.versNr ,oc.versNr )
  	assertEquals(oxx.getFields ,oc.getFields )
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


