package org.dbrain.data.cast;

import org.dbrain.data.text.ParseException;
import org.dbrain.data.util.Strings;

import java.text.DateFormat;

public class Dates {
  /**
   * Convert a string to a date supporting blank values as null date.
   */
  public static java.util.Date toDate(DateFormat format, String value ) {
      if ( !Strings.isBlank( value ) ) {
          try {
              return format.parse( value.trim() );
          } catch ( java.text.ParseException e ) {
              throw new ParseException( e );
          }
      } else {
          return null;
      }
  }
}
