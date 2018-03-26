package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.util.Strings;

public class Floats {
  /**
   * Cast String to float.
   */
  public static Float toFloat( String o ) {
      if ( Strings.isBlank( o ) ) {
          return null;
      }
      return Float.parseFloat( o.trim() );
  }

  /**
   * Cast object to Float.
   */
  public static Float toFloat( Object o ) {
      if ( o == null ) return null;
      if ( o instanceof Float ) return (Float) o;
      if ( o instanceof Number ) {
          return ( (Number) o ).floatValue();
      } else if ( o instanceof CharSequence ) {
          return toFloat( o.toString() );
      }
      throw new DataCoercionException( "Cannot cast " + o + " to Float." );
  }
}
