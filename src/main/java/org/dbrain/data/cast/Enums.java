package org.dbrain.data.cast;

import org.dbrain.data.util.Strings;

public class Enums {
  /**
   * Cast an enum to enum name, null safe.
   */
  public static String toEnumName( Enum e ) {
      return e != null ? e.name() : null;
  }

  /**
   * Cast a name to an enum. Null-safe.
   */
  public static <T extends Enum<T>> T toEnum( Class<T> enumClass, String name ) {
      return Strings.isBlank( name ) ? null : Enum.valueOf( enumClass, name.trim() );
  }
}
