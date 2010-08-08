# -*- coding: us-ascii -*-

# (C) 2010 by Florian Mayer
from collections import defaultdict

OPS = ['+', '-', '*', '/', '%', '!=', '==', '<=', '>=', '<', '>']
TYPES = ['Double', 'Float', 'Long', 'Int',
         'Short', 'Byte', 'BigInt', 'BigDecimal']

FMT_MAP = defaultdict(lambda: stdfmt)
PREFIX = """/*
 * (C) 2010 by Florian Mayer
 *
 * DO NOT MANUALLY CHANGE THIS FILE!
 * This has been created algorithmically with a Python script which can be
 * found in the repository.
 *
 * Extend the Scala library with a generic Number type which can hold a
 * number of any of the following types.
""" + "\n".join((" * - %s" % x for x in TYPES)) + """
 * Operations between these types are seamlessly provided.
 */

package number

object Prelude {
  def getTypeInfo(x: Any) = {
    if (x.isInstanceOf[Double])
      DOUBLE_NUMB
    else if (x.isInstanceOf[Float])
      FLOAT_NUMB
    else if (x.isInstanceOf[Long])
      LONG_NUMB
    else if (x.isInstanceOf[Short])
      SHORT_NUMB
    else if (x.isInstanceOf[Byte])
      BYTE_NUMB
    else if (x.isInstanceOf[Int])
      INT_NUMB
    else if (x.isInstanceOf[BigInt])
      BIGINT_NUMB
    else if (x.isInstanceOf[BigDecimal])
      BIGDECIMAL_NUMB
  }

  implicit def doubleToDoubleNum(x: Double) =
    new Number(x.asInstanceOf[AnyRef], DOUBLE_NUMB)
  implicit def floatToFloatNum(x: Float) =
    new Number(x.asInstanceOf[AnyRef], FLOAT_NUMB)
  implicit def longToLongNum(x: Long) =
    new Number(x.asInstanceOf[AnyRef], LONG_NUMB)
  implicit def shortToShortNum(x: Short) =
    new Number(x.asInstanceOf[AnyRef], SHORT_NUMB)
  implicit def byteToShortNum(x: Byte) =
    new Number(x.asInstanceOf[AnyRef], BYTE_NUMB)
  implicit def intToIntNumb(x: Int) =
    new Number(x.asInstanceOf[AnyRef], INT_NUMB)
  implicit def bigIntToBigIntNumb(x: BigInt) =
    new Number(x.asInstanceOf[AnyRef], BIGINT_NUMB)
  implicit def bigDecimalToBigDecimalNumb(x: BigDecimal) =
    new Number(x.asInstanceOf[AnyRef], BIGDECIMAL_NUMB)
  implicit def bigIntToBigDecimal(x: BigInt) = BigDecimal(x.toString)
  implicit def doubleToBigDecimal (x: Double) = BigDecimal(x)

  val COMMON_NUMB: Byte = 0
  val DOUBLE_NUMB: Byte = 1
  val FLOAT_NUMB: Byte = 2
  val LONG_NUMB: Byte = 3
  val INT_NUMB: Byte = 4
  val SHORT_NUMB: Byte = 5
  val BYTE_NUMB: Byte = 6
  val BIGINT_NUMB: Byte = 7
  val BIGDECIMAL_NUMB: Byte = 8
}

import Prelude._

class Number(val value: AnyRef, val typeinfo: Byte) {
"""
LINE_PREFIX = '\t'
POSTFIX = '}'

def stdfmt(op, this_type, other_type):
    return (
        "value.asInstanceOf[%s] %s other.value.asInstanceOf[%s]" %
        (this_type, op, other_type)
    )

def std(type_, fst):
    if fst:
        return "value.asInstanceOf[%s]" % type_
    else:
        return "other.value.asInstanceOf[%s]" % type_

def compose(fst=std, snd=std):
    def _fun(op, this_type, other_type):
        return fst(this_type, True) + ' ' + op + ' ' + snd(other_type, False)
    return _fun

def bigint(type_, fst):
    if fst:
        return "BigInt(value.asInstanceOf[%s].toString)" % type_
    else:
        return "BigInt(other.value.asInstanceOf[%s].toString)" % type_

def bigdecimal(type_, fst):
    if fst:
        return "BigDecimal(value.asInstanceOf[%s].toString)" % type_
    else:
        return "BigDecimal(other.value.asInstanceOf[%s].toString)" % type_


FMT_MAP['BigInt', 'BigDecimal'] = compose(bigdecimal)
FMT_MAP['BigDecimal', 'BigInt'] = compose(std, bigdecimal)

FMT_MAP['Double', 'BigInt'] = compose(bigdecimal, bigdecimal)
FMT_MAP['Double', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['Float', 'BigInt'] = compose(bigdecimal, bigdecimal)
FMT_MAP['Float', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['Long', 'BigInt'] = compose(bigint)
FMT_MAP['Long', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['Int', 'BigInt'] = compose(bigint)
FMT_MAP['Int', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['Byte', 'BigInt'] = compose(bigint)
FMT_MAP['Byte', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['Short', 'BigInt'] = compose(bigint)
FMT_MAP['Short', 'BigDecimal'] = compose(bigdecimal)

FMT_MAP['BigInt', 'Double'] = compose(bigdecimal)

FMT_MAP['BigInt', 'Float'] = compose(bigdecimal)

FWD_METHODS = [('override', 'toString'), ('', 'toDouble')]

def frmt(
    fmt_map, types, ops, fwd_methods, prefix="", postfix="", line_prefix=""
    ):
    res = prefix
    for op in ops:
        res += line_prefix + 'def %s(other: Number) = {\n' % op
        res += line_prefix + '\ttypeinfo match {\n'
        for own_type in types:
            res += line_prefix + '\t\tcase %s_NUMB => {\n' % own_type.upper()
            res += line_prefix + '\t\t\tother.typeinfo match {\n'
            for other_type in types:
                res += line_prefix + '\t\t\t\tcase %s_NUMB => {%s}\n' % (
                    other_type.upper(),
                    fmt_map[own_type, other_type](op, own_type, other_type)
                )
            res += line_prefix + '\t\t\t}\n'
            res += line_prefix + '\t\t}\n'
        res += line_prefix + '\t}\n'
        res += line_prefix + '}\n\t\n'
        
    for pfx, mth in fwd_methods:
        res += line_prefix + '%s def %s = typeinfo match {\n' % (pfx, mth)
        for type_ in types:
            res += line_prefix + '\t\tcase %s_NUMB => %s.%s\n' % (
                type_.upper(), std(type_, True), mth
            )
        res += line_prefix + '}\n\t\n'
    return res + postfix

if __name__ == '__main__':
    with open('mkc.txt', 'w') as fd:
        fd.write(
            frmt(
                FMT_MAP,
                TYPES, 
                OPS,
                FWD_METHODS,
                PREFIX,
                POSTFIX,
                LINE_PREFIX
            )
        )