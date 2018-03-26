package org.dbrain.data.cast;

public class Strings {
  /**
   * Cast object to String (null-safe).
   */
  public static String toString( Object o ) {
      return o != null ? o.toString() : null;
  }
}
