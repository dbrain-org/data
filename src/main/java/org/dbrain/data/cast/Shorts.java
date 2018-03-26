package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.DataTruncationException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class Shorts {
  /**
   * Cast BigDecimal to short.
   */
  public static Short toShort( BigDecimal bigDecimal ) {
      if ( bigDecimal == null ) {
          return null;
      }
      return bigDecimal.shortValueExact();
  }

  /**
   * cast string to short.
   */
  public static Short toShort( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      return toShort( new BigDecimal( o.trim() ) );
  }

  /**
   * Cast object to Short.
   */
  public static Short toShort( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Short ) return (Short) o;
      if ( o instanceof Byte ) {
          return ( (Number) o ).shortValue();
      } else if ( o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong) {
          long longValue = ( (Number) o ).longValue();
          if ( longValue >= Short.MIN_VALUE && longValue <= Short.MAX_VALUE ) {
              return (short) longValue;
          } else {
              throw new DataTruncationException();
          }
      } else if ( o instanceof Float || o instanceof Double ) {
          return new BigDecimal( ( (Number) o ).doubleValue() ).shortValueExact();
      } else if ( o instanceof BigDecimal ) {
          return toShort( (BigDecimal) o );
      } else if ( o instanceof BigInteger) {
          BigInteger bigInteger = (BigInteger) o;
          return bigInteger.shortValueExact();
      } else if ( o instanceof CharSequence ) {
          return toShort( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Short." );
  }
}
