package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

/**
 * Methods used to cast to BigDecimal
 *
 * Unless noted, those functions handle null fluently so you can call func1(func2(func3(x))) without
 * worrying that x might be null.
 */
public class BigDecimals {

  /**
   * Cast string to BigDecimal.
   */
  public static BigDecimal toBigDecimal(String o) {
    if (Strings.isBlank(o)) return null;
    return new BigDecimal(o.trim());
  }

  /**
   * Cast object to BigDecimal.
   */
  public static BigDecimal toBigDecimal(Object o) {
    if (o == null) return null;
    if (o instanceof BigDecimal) return (BigDecimal) o;
    if (o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong) {
      return new BigDecimal(((Number) o).longValue());
    } else if (o instanceof Float || o instanceof Double) {
      return new BigDecimal(((Number) o).doubleValue());
    } else if (o instanceof BigInteger) {
      return new BigDecimal((BigInteger) o);
    } else if (o instanceof CharSequence) {
      return toBigDecimal(o.toString());
    }
    throw new DataCoercionException("Cannot cast " + o + " to BigDecimal.");
  }

  /**
   * Function to set the precision and scale of a BigDecimal.
   *
   * @param precision The precision.
   * @param scale The scale.
   * @param rounding The rounding mode.
   * @return
   */
  public static Function<BigDecimal, BigDecimal> setDecimalParameters(final int precision,
                                                                      final int scale,
                                                                      final RoundingMode rounding ) {
      return new Function<BigDecimal, BigDecimal>() {

          final MathContext mathContext = new MathContext( precision, rounding );

          @Override
          public BigDecimal apply( BigDecimal bigDecimal ) {
              if ( bigDecimal == null ) {
                  return null;
              }
              return bigDecimal.setScale( scale, mathContext.getRoundingMode() );
          }
      };
  }
}
