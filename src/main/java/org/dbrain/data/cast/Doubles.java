package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

public class Doubles {
  /**
   * Cast String to double.
   */
  public static Double toDouble( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      return Double.parseDouble( o.trim() );
  }

  /**
   * Cast object to Double.
   */
  public static Double toDouble( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Double ) return (Double) o;
      if ( o instanceof Number ) {
          return ( (Number) o ).doubleValue();
      } else if ( o instanceof CharSequence ) {
          return toDouble( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Double." );
  }
}
