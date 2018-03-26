package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

public class Booleans {
  private static Pattern NUMBER_PATTERN = Pattern.compile( "\\d+(\\.\\d+)?" );

  /**
   * Cast string to boolean.
   */
  public static Boolean toBoolean( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      o = o.trim();
      if ( NUMBER_PATTERN.matcher( o ).matches() ) {
          return !( BigDecimals.toBigDecimal( o ).compareTo( BigDecimal.ZERO ) == 0 );
      }
      if ( Boolean.TRUE.toString().equalsIgnoreCase( o ) ) {
          return true;
      } else if ( Boolean.FALSE.toString().equalsIgnoreCase( o ) ) {
          return false;
      } else {
          throw new DataCoercionException( "Cannot cast '" + o + "' to boolean." );
      }
  }

  /**
   * Cast object to Boolean.
   */
  public static Boolean toBoolean( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Boolean ) return (Boolean) o;
      if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong) {
          return ( (Number) o ).longValue() != 0;
      }
      if ( o instanceof Float || o instanceof Double ) {
          return ( (Number) o ).doubleValue() != 0;
      } else if ( o instanceof BigDecimal ) {
          return !( ( (BigDecimal) o ).compareTo( BigDecimal.ZERO ) == 0 );
      } else if ( o instanceof BigInteger) {
          return !( ( (BigInteger) o ).compareTo( BigInteger.ZERO ) == 0 );
      } else if ( o instanceof CharSequence ) {
          return toBoolean( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Boolean." );
  }
}
