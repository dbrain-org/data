package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class BigIntegers {
  /**
   * Cast String to BigInteger.
   */
  public static BigInteger toBigInteger(String o ) {
      if ( Strings.isBlank( o ) ) return null;
      return new BigInteger( o.trim() );
  }

  /**
   * Cast object to BigInteger.
   */
  public static BigInteger toBigInteger( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof BigInteger ) return (BigInteger) o;
      if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong) {
          return new BigInteger( o.toString() );
      }
      if ( o instanceof Float || o instanceof Double ) {
          return new BigDecimal( ( (Number) o ).doubleValue() ).toBigIntegerExact();
      }
      if ( o instanceof BigDecimal ) {
          return ( (BigDecimal) o ).toBigIntegerExact();
      }
      if ( o instanceof CharSequence ) {
          return toBigInteger( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to BigInteger." );
  }
}
