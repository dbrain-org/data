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

package org.dbrain.data.text;

import org.dbrain.data.Casts;
import org.dbrain.data.util.Strings;

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;

/**
 * Formatter for LocalDate.
 */
public class LocalDateFormat implements Format<LocalDate> {

    private final SimpleDateFormat dateFormat;

    public LocalDateFormat( String pattern, Locale locale ) {
        dateFormat = new SimpleDateFormat( pattern, locale );
        dateFormat.setLenient( false );
    }

    @Override
    public synchronized String format( LocalDate value ) throws FormatException {
        return value != null ? dateFormat.format( Date.from( value.atStartOfDay( ZoneId.systemDefault() ).toInstant() ) ) : null;
    }

    @Override
    public synchronized LocalDate parse( String value ) throws ParseException {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            String trimmedValue = value.trim();

            ParsePosition pos = new ParsePosition( 0 );
            Date result = dateFormat.parse( trimmedValue, pos );
            if ( pos.getIndex() != trimmedValue.length() ) {
                int index = pos.getErrorIndex() >= 0 ? pos.getErrorIndex() : pos.getIndex();
                throw new ParseException( "Error parsing date [" + trimmedValue + "] at position " + index + "." );
            }
            return Casts.toLocalDate( result );
        }
    }

}
