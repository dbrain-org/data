package org.dbrain.data.cast;

import org.dbrain.data.text.ParseException;
import org.dbrain.data.util.Strings;

import java.text.DateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

public class Dates {

  /**
   * Convert a string to a date supporting blank values as null date.
   */
  public static Date toDate(DateFormat format, String value ) {
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

  /**
   * Convert a local date to date in the specific time zone.
   * @param localDate The local date
   * @param zoneId The time zone.
   * @return A date object.
   */
  public static Date toDate(LocalDate localDate, ZoneId zoneId ) {
      if ( localDate != null ) {
          return Date.from(localDate.atStartOfDay(zoneId).toInstant());
      } else {
          return null;
      }
  }
}
