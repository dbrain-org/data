package org.dbrain.data.cast;

import java.text.DateFormat;
import java.time.LocalDate;

public class SqlDates {
  /**
   * Convert a string to a sql date supporting blank values as null date.
   */
  public static java.sql.Date toSqlDate(DateFormat format, String value ) {
      return toSqlDate( Dates.toDate( format, value ) );
  }

  /**
   * Cast date to sql date.
   */
  public static java.sql.Date toSqlDate( java.util.Date date ) {
      return date != null ? new java.sql.Date( date.getTime() ) : null;
  }

  /**
   * Cast date to sql date.
   */
  public static java.sql.Date toSqlDate( LocalDate date ) {
      return date != null ? java.sql.Date.valueOf( date ) : null;
  }
}
