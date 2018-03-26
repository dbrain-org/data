package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.DataTruncationException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class Integers {
  /**
   * Cast BigDecimal to integer.
   */
  public static Integer toInteger( BigDecimal bigDecimal ) {
      if ( bigDecimal == null ) {
          return null;
      }
      return bigDecimal.intValueExact();
  }

  /**
   * Cast string to integer.
   */
  public static Integer toInteger( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      return toInteger( new BigDecimal( o.trim() ) );
  }

  /**
   * Cast object to integer.
   */
  public static Integer toInteger( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Integer ) return (Integer) o;
      if ( o instanceof Byte || o instanceof Short || o instanceof AtomicInteger) {
          return ( (Number) o ).intValue();
      } else if ( o instanceof Long || o instanceof AtomicLong) {
          long longValue = ( (Number) o ).longValue();
          if ( longValue >= Integer.MIN_VALUE && longValue <= Integer.MAX_VALUE ) {
              return (int) longValue;
          } else {
              throw new DataTruncationException();
          }
      } else if ( o instanceof Float || o instanceof Double ) {
          return new BigDecimal( ( (Number) o ).doubleValue() ).intValueExact();
      } else if ( o instanceof BigDecimal ) {
          return toInteger( (BigDecimal) o );
      } else if ( o instanceof BigInteger) {
          BigInteger bigInteger = (BigInteger) o;
          return bigInteger.intValueExact();
      } else if ( o instanceof CharSequence ) {
          return toInteger( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Integer." );
  }
}
