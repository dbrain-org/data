package epic.data.adapters;

import epic.data.Adapter;

import java.sql.Time;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Adapters for dates.
 */
public class DateAdapters {

    /**
     * Transform a date to Sql date.
     */
    public static final Adapter<Date, java.sql.Date> DATE_TO_SQL_DATE = new Adapter<Date, java.sql.Date>() {
        @Override
        public java.sql.Date apply( Date date ) {
            return date == null ? null : new java.sql.Date( date.getTime() );
        }
    };

    /**
     * Transform a date to Sql time.
     */
    public static final Adapter<Date, java.sql.Time> DATE_TO_SQL_TIME = new Adapter<Date, Time>() {
        @Override
        public Time apply( Date date ) {
            return date == null ? null : new Time( date.getTime() );
        }
    };

    /**
     * Transform a sql date to date.
     */
    public static final Adapter<java.sql.Date, Date> SQL_DATE_TO_DATE = new Adapter<java.sql.Date, Date>() {
        @Override
        public Date apply( java.sql.Date date ) {
            return date == null ? null : new Date( date.getTime() );
        }
    };

    /**
     * Transform a sql time to date.
     */
    public static final Adapter<java.sql.Time, Date> SQL_TIME_TO_DATE = new Adapter<java.sql.Time, Date>() {
        @Override
        public Date apply( java.sql.Time time ) {
            return time == null ? null : new Date( time.getTime() );
        }
    };

}
