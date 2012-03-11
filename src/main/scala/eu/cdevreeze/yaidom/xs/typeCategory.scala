package eu.cdevreeze.yaidom
package xs

sealed abstract class TypeCategory(val shortValue: Short)

object TypeCategory {

  object DTAnySimpleType extends TypeCategory(1)
  object DTString extends TypeCategory(2)
  object DTBoolean extends TypeCategory(3)
  object DTDecimal extends TypeCategory(4)
  object DTFloat extends TypeCategory(5)
  object DTDouble extends TypeCategory(6)
  object DTDuration extends TypeCategory(7)
  object DTDateTime extends TypeCategory(8)
  object DTTime extends TypeCategory(9)
  object DTDate extends TypeCategory(10)
  object DTGYearMonth extends TypeCategory(11)
  object DTYear extends TypeCategory(12)
  object DTGMonthDay extends TypeCategory(13)
  object DTGDay extends TypeCategory(14)
  object DTGMonth extends TypeCategory(15)
  object DTHexBinary extends TypeCategory(16)
  object DTBase64Binary extends TypeCategory(17)
  object DTAnyUri extends TypeCategory(18)
  object DTQName extends TypeCategory(19)
  object DTNotation extends TypeCategory(20)
  object DTNormalizedString extends TypeCategory(21)
  object DTToken extends TypeCategory(22)
  object DTLanguage extends TypeCategory(23)
  object DTNMToken extends TypeCategory(24)
  object DTName extends TypeCategory(25)
  object DTNCName extends TypeCategory(26)
  object DTId extends TypeCategory(27)
  object DTIdRef extends TypeCategory(28)
  object DTEntity extends TypeCategory(29)
  object DTInteger extends TypeCategory(30)
  object DTNonPositiveInteger extends TypeCategory(31)
  object DTNegativeInteger extends TypeCategory(32)
  object DTLong extends TypeCategory(33)
  object DTInt extends TypeCategory(34)
  object DTShort extends TypeCategory(35)
  object DTByte extends TypeCategory(36)
  object DTNonNegativeInteger extends TypeCategory(37)
  object DTUnsignedLong extends TypeCategory(38)
  object DTUnsignedInt extends TypeCategory(39)
  object DTUnsignedShort extends TypeCategory(40)
  object DTUnsignedByte extends TypeCategory(41)
  object DTPositiveInteger extends TypeCategory(42)
  object DTListOfUnion extends TypeCategory(43)
  object DTList extends TypeCategory(44)
  object DTUnavailable extends TypeCategory(45)

  val all: Set[TypeCategory] = Set(
    DTAnySimpleType,
    DTString,
    DTBoolean,
    DTDecimal,
    DTFloat,
    DTDouble,
    DTDuration,
    DTDateTime,
    DTTime,
    DTDate,
    DTGYearMonth,
    DTYear,
    DTGMonthDay,
    DTGDay,
    DTGMonth,
    DTHexBinary,
    DTBase64Binary,
    DTAnyUri,
    DTQName,
    DTNotation,
    DTNormalizedString,
    DTToken,
    DTLanguage,
    DTNMToken,
    DTName,
    DTNCName,
    DTId,
    DTIdRef,
    DTEntity,
    DTInteger,
    DTNonPositiveInteger,
    DTNegativeInteger,
    DTLong,
    DTInt,
    DTShort,
    DTByte,
    DTNonNegativeInteger,
    DTUnsignedLong,
    DTUnsignedInt,
    DTUnsignedShort,
    DTUnsignedByte,
    DTPositiveInteger,
    DTListOfUnion,
    DTList,
    DTUnavailable)

  def fromShortValue(shortValue: Short): TypeCategory = {
    all find { tpe => tpe.shortValue == shortValue } getOrElse (sys.error("No type category found with short value %d".format(shortValue)))
  }
}
