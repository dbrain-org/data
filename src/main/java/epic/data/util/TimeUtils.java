package epic.data.util;

import java.sql.Time;
import java.util.Date;

/**
 * Adapters for dates.
 */
public class TimeUtils {

    /**
     * Transform a date to Sql date.
     */
    public java.sql.Date toSqlDate( Date date ) {
        return date == null ? null : new java.sql.Date( date.getTime() );
    }

    /**
     * Transform a date to Sql time.
     */
    public Time toSqlTime( Date date ) {
        return date == null ? null : new Time( date.getTime() );
    }

    /**
     * Transform a date to a (new) date.
     */
    public Date toDate( Date date ) {
        return date == null ? null : new Date( date.getTime() );
    }

}
