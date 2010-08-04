/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

/**
 * Enum of all possible data types for an expression and data field
 */
object DataType extends Enumeration 
{	
	val IntTyp=Value("Integer")
	val LongTyp=Value("Long")
	val DoubleTyp=Value("Double")
	val CurrencyTyp=Value("Currency")
	val StringTyp=Value("String")
	val BoolTyp=Value("Boolean")
	val DateTyp=Value("Date")
	val ObjectRefTyp=Value("ObjRef")	
	val undefined=Value("UNDEFINED")
	val BinOp=Value("BinOp")
	val FieldRefTyp=Value("FieldRef")
}
