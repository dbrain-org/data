package org.dbrain.data.cast;

import java.text.DateFormat;

public class SqlTimes {
  /**
   * Cast date to sql time.
   */
  public static java.sql.Time toSqlTime( java.util.Date date ) {
      return date != null ? new java.sql.Time( date.getTime() ) : null;
  }

  /**
   * Cast string to sql time.
   */
  public static java.sql.Time toSqlTime(DateFormat format, String time ) {
      return toSqlTime( Dates.toDate( format, time ) );
  }
}
