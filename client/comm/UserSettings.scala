/**
 * Author: Peter Started:27.09.2010
 */
package client.comm

import java.io._
import definition.data._
import scala.collection.mutable.{Map,HashMap}
import scala.util.parsing.combinator._
import scala.collection.immutable.Seq

/** reads and stores user settings
 * 
 */

abstract class PropertyValue{
	def name:String
	def writeString:String
	def value:Any
}

case class IntValue(override val name:String,override val value:Int) extends PropertyValue {
	def writeString= "{"+UserSettings.quoteString(name)+":"+value+"}"
}

case class StringValue(override val name:String,override val value:String) extends PropertyValue {
	def writeString="{"+UserSettings.quoteString(name)+":"+UserSettings.quoteString(value)+"}"
}

case class ListValue[T](override val name:String,override val value:collection.Seq[T]) extends PropertyValue {
	def writeString="{"+UserSettings.quoteString(name)+": ["+value.map {
		_ match {
			case i:Int => i.toString
			case s:String => UserSettings.quoteString(s)	
			case r:Reference => r.sToString()
			case e:Tuple2[_,_] => "("+e._1+":"+e._2+")"
		}
	}.mkString(",")+"] }"	
}

case class PropertyGroup(name:String,val properties:collection.mutable.Map[String,PropertyValue]) {
	def writeString="\n{"+UserSettings.quoteString(name)+":\n"+properties.valuesIterator.map {
		_.writeString	}.mkString("\n")+ "}"
}



// *****************************************************************************************************

object UserSettings {
	private var groupList=HashMap[String,PropertyGroup]()

	def loadProperties(in:DataInput) = {

	}

	def writeProperties:String = {
		"UserSettings {"+groupList.valuesIterator.map{_.writeString}.mkString("\n")+"}"
	}

	private def internGetValue[T ](groupName:String,name:String,defaultValue:T):T = {
		if(groupList.contains(groupName)) {
			val group=groupList(groupName)
			if(group.properties.contains(name)) try { 
				group.properties(name).value.asInstanceOf[T]
			} catch {case e:Exception => return defaultValue }
			else defaultValue
		} else defaultValue
	}

	def getStringProperty(group:String,name:String,defaultValue:String=""):String = 
		internGetValue[String](group,name,defaultValue)	

	def getIntProperty(group:String,name:String,defaultValue:Int=0):Int =
			internGetValue[Int](group,name,defaultValue)

	def getListProperty[T](group:String,name:String,defaultValue:Seq[T]=Seq()): Seq[T] = 
				internGetValue[collection.immutable.Seq[T]](group,name,defaultValue)		

	private def getGroup(group:String):PropertyGroup	= {
		if (groupList.contains(group)) groupList(group)
		else {
			val newGroup=new PropertyGroup(group,HashMap())
			groupList(group)= newGroup
			newGroup
		}
	}

	def setStringProperty(group:String,name:String,value:String) =
		getGroup(group).properties(name)=new StringValue(name,value)

	def setIntProperty(group:String,name:String,value:Int) =
		getGroup(group).properties(name)=new IntValue(name,value)

	def setListProperty[T](group:String,name:String,value:collection.Seq[T]) =
		getGroup(group).properties(name)=new ListValue[T](name,value)	

	def removeProperty(group:String,name:String) = getGroup(group).properties.remove(name)	
		

		def quoteString(s: String) = {
		val charCount = s.codePointCount(0, s.length)
		"\"" + (0 until charCount).map { idx =>
		s.codePointAt(s.offsetByCodePoints(0, idx)) match {
			case 0x0d => "\\r"
			case 0x0a => "\\n"
			case 0x09 => "\\t"
			case 0x22 => "\\\""        
			case 0x2f => "\\/"
			case 0x5c => "\\\\"
			case c => quoteChar(c)
		}
		}.mkString("") + "\""
	}

	def quoteChar(value: Int) = {
		value match {
			case c if (c > 0xffff) =>
			val chars = Character.toChars(c)
			"\\u%04x\\u%04x".format(chars(0).toInt, chars(1).toInt)
			case c if c > 0x7e => "\\u%04x".format(c.toInt)
			case c => c.toChar
		}
	}
	
	def parse(s: String) = {
		lazy val emptyMap=HashMap[String,PropertyGroup]()
    groupList=if(s.length==0) emptyMap  
    	else {
    		val result=SettingsParser.parseAll(SettingsParser.allSettings, s)
    		result match {
            case SettingsParser.Success(x, _) => x
            case SettingsParser.NoSuccess(err, next) => {
            		print("Failure when parsing "+                
                    "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
                    err + "\n" + next.pos.longString)
                emptyMap
            }
        }
    	}
  }

	private object SettingsParser extends JavaTokenParsers {
		def intNumber: Parser[Int] = """-?\d+""".r ^^ { case x => x.toInt }

		def unicode: Parser[String] = rep1("\\u" ~> """[a-fA-F0-9]{4}""".r) ^^ { stringBytes =>
			new String(stringBytes.map(Integer.valueOf(_, 16).intValue.asInstanceOf[Char]).toArray)
		}

		def escaped: Parser[String] = "\\" ~> """[\\/bfnrt"]""".r ^^ { charStr =>
			val char = charStr match {
				case "r" => '\r'
				case "n" => '\n'
				case "t" => '\t'
				case "b" => '\b'
				case "f" => '\f'
				case x => x.charAt(0)
			}
			char.toString
		}

		def characters: Parser[String] = """[^\"[\x00-\x1F]\\]+""".r

		def string: Parser[String] =
			"\"" ~> rep(unicode | escaped | characters) <~ "\"" ^^ { list =>
			list.mkString("")
		}
			
		def reference: Parser[Reference] = (("(" ~> intNumber ) ~ ("," ~> intNumber) <~ ")") ^^ {
			case typ ~ inst => new Reference(typ,inst) 
		}
		
		def tuple: Parser[Tuple2[Int,Int]] = (("(" ~> intNumber ) ~ (":" ~> intNumber) <~ ")") ^^ {
			case typ ~ inst => (typ,inst) 
		}

		def intVal:Parser[IntValue] = ("{" ~> (string <~ ":") ~ (intNumber <~"}")) ^^ { 
			case name ~ value => new IntValue (name,value) 
		}
		
		def stringVal:Parser[StringValue] = ("{" ~> (string <~ ":") ~ (string <~"}")) ^^ { 
			case name ~ value => new StringValue (name,value) 
		}
		
		def intListVal: Parser[ListValue[Int]] = ("{" ~> (string <~ ": [") ~ repsep(intNumber, ",") <~ "] }") ^^ {
			case name ~ list =>	new ListValue(name,list)			
		}
		
		def stringListVal: Parser[ListValue[String]] = ("{" ~> (string <~ ": [") ~ repsep(string, ",") <~ "] }") ^^ {
			case name ~ list => new ListValue(name,list)  
		}
		
		def refListVal: Parser[ListValue[Reference]] = ("{" ~> (string <~ ": [") ~ repsep(reference, ",") <~ "] }") ^^ {
			case name ~ list => new ListValue(name,list)  
		}
		
		def tupleListVal: Parser[ListValue[Tuple2[Int,Int]]] = ("{" ~> (string <~ ": [") ~ repsep(tuple, ",") <~ "] }") ^^ {
			case name ~ list => new ListValue(name,list)  
		}
		
		def propertyGroup:Parser[PropertyGroup] = ("{" ~> (string <~ ":\n") ~ 
				(rep(intVal|stringVal|intListVal|stringListVal|refListVal|tupleListVal) <~"}")) ^^ {
			case name ~ list => {				
				val n=HashMap[String,PropertyValue](list.map(a => (a.name , a)) : _*)
				//val hm=new collection.mutable.HashMap[String,PropertyValue]()++m
				new PropertyGroup(name,n) 
			}
		}
		
		def allSettings:Parser[HashMap[String,PropertyGroup]] = ("UserSettings {" ~> rep(propertyGroup)) <~ "}" ^^ {
			case list => {
				HashMap[String,PropertyGroup](list.map(a => (a.name,a)) : _*)
			}
		}
	}


  def main (args:Array[String]):Unit = {
  	UserSettings.setIntProperty("testgroup","intvalue",12)
  	UserSettings.setIntProperty("other group","another intvalue",144)
  	UserSettings.setStringProperty("other group","a name","duda")
  	UserSettings.setListProperty("testgroup","an intlist",Seq(4,55,-99))
  	UserSettings.setListProperty("other group","a stringlist",Seq("hallo "," du ", "da hier"))
  	UserSettings.setListProperty("other group","a reflist",Seq(Reference(1,2),Reference(3,4)))
  	val ostr=UserSettings.writeProperties
  	println(ostr)
  	//println(UserSettings.parse(ostr).mkString("\n"))
  }
}


