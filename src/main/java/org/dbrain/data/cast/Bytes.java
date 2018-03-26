package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.DataTruncationException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class Bytes {
  /**
   * Cast BigDecimal to byte.
   */
  public static Byte toByte( BigDecimal bigDecimal ) {
      if ( bigDecimal == null ) {
          return null;
      }
      return bigDecimal.byteValueExact();
  }

  /**
   * Cast string to byte.
   */
  public static Byte toByte( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      return toByte( new BigDecimal( o.trim() ) );
  }

  /**
   * Cast object to byte.
   */
  public static Byte toByte( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Byte ) return (Byte) o;
      if ( o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong) {
          long longValue = ( (Number) o ).longValue();
          if ( longValue >= Byte.MIN_VALUE && longValue <= Byte.MAX_VALUE ) {
              return (byte) longValue;
          } else {
              throw new DataTruncationException();
          }
      } else if ( o instanceof Float || o instanceof Double ) {
          return new BigDecimal( ( (Number) o ).doubleValue() ).byteValueExact();
      } else if ( o instanceof BigDecimal ) {
          return toByte( (BigDecimal) o );
      } else if ( o instanceof BigInteger) {
          BigInteger bigInteger = (BigInteger) o;
          return bigInteger.byteValueExact();
      } else if ( o instanceof CharSequence ) {
          return toByte( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Byte." );
  }
}
