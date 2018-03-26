package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class Longs {
  /**
   * Cast BigDecimal to Long.
   */
  public static Long toLong( BigDecimal bigDecimal ) {
      if ( bigDecimal == null ) {
          return null;
      }
      return bigDecimal.longValueExact();
  }

  /**
   * Cast object to long.
   */
  public static Long toLong( String o ) {
      if ( Strings.isBlank( o ) ) return null;
      return toLong( new BigDecimal( o.trim() ) );
  }

  /**
   * Cast object to long.
   */
  public static Long toLong( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Long ) return (Long) o;
      if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof AtomicInteger || o instanceof AtomicLong) {
          return ( (Number) o ).longValue();
      } else if ( o instanceof Float || o instanceof Double ) {
          return new BigDecimal( ( (Number) o ).doubleValue() ).longValueExact();
      } else if ( o instanceof BigDecimal ) {
          return toLong( (BigDecimal) o );
      } else if ( o instanceof BigInteger) {
          BigInteger bigInteger = (BigInteger) o;
          return bigInteger.longValueExact();
      } else if ( o instanceof CharSequence ) {
          return toLong( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Long." );
  }
}
