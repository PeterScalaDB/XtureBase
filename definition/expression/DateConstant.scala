/**
 * Author: Peter Started:24.04.2011
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataOutput,DataInput}
import java.util.{Calendar,Date}
import java.text.DateFormat
import java.util.GregorianCalendar


/**
 * 
 */
case class DateSet(day:Int,month:Int,year:Int){
	
}

case class DateConstant(val data:Date) extends Constant {		
	
	lazy val dateSet=createDateSet
	lazy val day=dateSet.day
	lazy val month=dateSet.month
	lazy val year=dateSet.year
		
	def createDateSet= {
		DateConstant.calendar.setTime(data);
		new DateSet(DateConstant.calendar.get(Calendar.DATE),DateConstant.calendar.get(Calendar.MONTH)+1,DateConstant.calendar.get(Calendar.YEAR))
	}
	
	lazy val julian =	{
		var m:Long=dateSet.month
		var t:Long=dateSet.day
		var j:Long=dateSet.year		
		if ( m>2 ) m -= 3   
		else {  
			m += 9;
			j -= 1
		}
		t += (153*m+2)/5;
		val c = (146097L*((j) / 100L))/4L;
		val y =   (1461L*((j) % 100L))/4L;
		c+y+t+1721119L;
	}
	
	
	def addDays(ndays:Int):DateConstant = DateConstant.asJulian(julian+ndays)
	
	def dayDistance(other:DateConstant):Int = (other.julian-julian).toInt
	
	def dayInYear():Int = {   
   val d = (dateSet.month+10)/13;
   val e = dateSet.day + (611*(dateSet.month+2))/20 - 2*d - 91;
   e + DateConstant.leapYear(dateSet.year)*d; 
	}
	
	def getWeekDay = DateConstant.getWeekDayInYear(dateSet.year,dayInYear())+1;
	
	// *********************** interface Constant ************************************
	
	
  def toInt(): Int = { 0 }

  def toLong(): Long = { data.getTime}

  def toDouble(): Double = { 0.0d }

  def toBoolean(): Boolean = { false }

  def getNative(): Any = { data }

  def getType(): DataType.Value = { DataType.DateTyp  }

  def createCopy(): Expression = { new DateConstant(new Date(data.getTime)) }

  def getTerm(): String = { toString }
  
  override def toString=DateConstant.shortDateTimeFormat.format(data)

  def write(file: DataOutput): Unit = {
  	file.writeByte(DataType.DateTyp.id) 
  	file.writeLong(data.getTime) }
  
  override def toDate=data
  
  def sameDay(other:DateConstant)= dateSet.year ==other.dateSet.year && dateSet.month ==other.dateSet.month &&
     dateSet.day ==other.dateSet.day 
}

object DateConstant {	
	lazy val shortDateFormat= DateFormat.getDateInstance(DateFormat.SHORT);
	lazy val shortTimeFormat= DateFormat.getTimeInstance(DateFormat.SHORT);
	lazy val shortDateTimeFormat=DateFormat.getDateTimeInstance(DateFormat.SHORT,DateFormat.SHORT);
	lazy val calendar=new GregorianCalendar
	
	def apply(file:DataInput)= new DateConstant(new Date(file.readLong))
	def apply(day:Int,month:Int,year:Int)= {
		calendar.set(day,month-1,year)
		new DateConstant(calendar.getTime)
	}
	val nativeNull=new Date(0)
	
	def asJulian(jday:Long)=
	{		
		var jd =jday- 1721119L
		var j  = (4L*jd-1L) / 146097L
		jd = (4L*jd-1L) % 146097L
		var t  = jd/4L
		jd = (4L*t+3L) / 1461L
		t  = (4L*t+3L) % 1461L
		t  = (t+4L)/4L;
		var m  = (5L*t-3L) / 153L
		t  = (5L*t-3L) % 153L
		t  = (t+5L)/5L
		j  = 100L*j + jd
		if ( m < 10L ) m+=3   
		else   {
			m-=9
			j+=1
		}
		apply( t.toInt,m.toInt,j.toInt)
	}
	
	def leapYear(year:Int):Int = {
		if ( ((year % 4 == 0) && (year % 100 != 0))
				|| (year % 400 == 0) )
			return 1
			return 0
	}

	def getWeekDayInYear(year:Int, n:Int)= {   
		val j = (year-1) % 100;
		val c = (year-1) / 100;
		((28+j+n+(j/4)+(c/4)+5*c) % 7) 
	}
}