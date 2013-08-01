package epic.data.adapters;

import epic.data.Adapter;

import java.util.Date;

/**
 * Adapters that works starting with date.
 */
public class DateAdapters {

    /**
     * Transform a date to sql.Date.
     */
    public static final Adapter<Date, java.sql.Date> TO_SQL_DATE = new Adapter<Date, java.sql.Date>() {
        @Override
        public java.sql.Date apply( Date date ) {
            if ( date == null ) {
                return null;
            }
            return new java.sql.Date( date.getTime() );
        }
    };


}
