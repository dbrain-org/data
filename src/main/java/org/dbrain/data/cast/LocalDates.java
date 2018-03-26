package org.dbrain.data.cast;

import org.dbrain.data.DataCoercionException;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

public class LocalDates {
  /**
   * Convert a date to LocalDate.
   */
  public static LocalDate toLocalDate(Date date, ZoneId zoneId ) {
      if ( date != null ) {
          if ( date instanceof java.sql.Date ) {
              return ( (java.sql.Date) date ).toLocalDate();
          } else if ( date instanceof java.sql.Time ) {
              throw new DataCoercionException( "Cannot case " + date + " to LocalDate." );
          } else {
              return date.toInstant().atZone( zoneId ).toLocalDate();
          }
      } else {
          return null;
      }
  }

  /**
   * Convert a date to local date using system's timezone.
   */
  public static LocalDate toLocalDate( Date date ) {
      return toLocalDate( date, ZoneId.systemDefault() );
  }
}
