/*
 * Copyright [2014] [Eric Poitras]
 *
 *     Licensed under the Apache License, Version 2.0 (the "License");
 *     you may not use this file except in compliance with the License.
 *     You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dbrain.data.type.impl;

import org.dbrain.data.util.Strings;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Base type for date values. The date returned by this type is always in the timezone of the system except if explicitly noted in
 * the function`s specifications.
 */
public class TdTimeStamp extends TemporalType<Date, Operations.Temporal<Date>> {

    private RawOperations rawOps = new RawOperations();

    /**
     * The date format to use to coalesce (and parse?) a string into a date. If null, then the local format will be used.
     */
    private String dateFormat;

    /**
     * Create a new date.
     */
    protected TdTimeStamp() {
    }

    /**
     * Create a new date that will use a non-local date format.
     *
     * @param dateFormatPattern
     */
    public TdTimeStamp( String dateFormatPattern ) {
        this.dateFormat = dateFormatPattern;
    }

    protected TimeZone getTimeZone() {
        return TimeZone.getDefault();
    }

    protected Locale getLocale() {
        return Locale.getDefault();
    }

    protected DateFormat getDateFormat() {
        DateFormat result;
        if ( dateFormat == null ) {
            result = DateFormat.getDateInstance( DateFormat.SHORT, getLocale() );
        } else {
            result = new SimpleDateFormat( dateFormat );
        }
        result.setTimeZone( getTimeZone() );
        result.setLenient( false );

        return result;
    }

    protected Calendar createCalendar() {
        return new GregorianCalendar( getTimeZone(), getLocale() );
    }

    protected Date processDate( Date toProcess ) {
        Calendar c = createCalendar();
        c.setTime( toProcess );
        return c.getTime();
    }

    @Override
    public Operations.Temporal<Date> getRawOperations() {
        return rawOps;
    }

    @Override
    public Class<Date> getBaseClass() {
        return Date.class;
    }

    @Override
    public Date cast( Object value ) {
        if ( value == null ) {
            return null;
        }

        value = TypeUtil.objectGetSingle( value );
        if ( value instanceof Calendar ) {
            return cast( (Calendar) value );
        }
        if ( value instanceof Date ) {
            return cast( (Date) value );
        }
        if ( value instanceof Long ) {
            return cast( (Long) value );
        }
        if ( value instanceof String ) {
            return cast( (String) value );
        }

        throw new TypeCastException( this, value );
    }

    public Date cast( int year, int month, int day ) {
        Calendar c = createCalendar();
        c.set( Calendar.YEAR, year );
        c.set( Calendar.MONTH, month - 1 );
        c.set( Calendar.DAY_OF_MONTH, day );
        return cast( c );
    }

    public Date cast( Date value ) {
        if ( value == null ) {
            return null;
        }

        return processDate( value );
    }

    public Date cast( Calendar value ) {
        if ( value == null ) {
            return null;
        }

        return cast( value.getTime() );
    }

    public Date cast( Long value ) {
        if ( value == null ) {
            return null;
        }

        return cast( new Date( value ) );
    }

    public Date cast( String value ) {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            try {
                Date date = getDateFormat().parse( value );
                return cast( date );
            } catch ( Exception ex ) {
                throw new TypeCastException( this, value, ex );
            }
        }
    }

    private class RawOperations implements Operations.Temporal<Date> {

        /**
         * Convert the provided getValue to date, following the current type restrictions.
         *
         * @param value
         * @return
         */
        public Date toDate( Date value ) {
            return cast( value );
        }

        public String toString( Date value ) {
            Date date = cast( value );
            return date != null ? getDateFormat().format( cast( value ) ) : null;
        }

        public boolean equals( Date base1, Date base2 ) {
            return compare( base1, base2 ) == 0;
        }

        public int compare( Date base1, Date base2 ) {
            // Handle Null values
            if ( base1 == null ) {
                if ( base2 == null ) {
                    return 0;
                } else {
                    return -1;
                }
            }
            if ( base2 == null ) {
                return 1;
            }

            // Compare the two dates
            return base1.compareTo( base2 );
        }

    }
}
