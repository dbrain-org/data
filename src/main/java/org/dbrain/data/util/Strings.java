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

package org.dbrain.data.util;

import org.dbrain.data.DataTruncationException;

import java.util.Comparator;
import java.util.Locale;

/**
 * Contains static methods to manipulate strings.
 */
public class Strings {

    /**
     * Natural order for strings.
     */
    public static final Comparator<String> NATURAL_ORDER = ( o1, o2 ) -> {
        if ( o1 == null ) {
            return o2 == null ? 0 : -1;
        } else {
            return o2 == null ? 1 : o1.compareTo( o2 );
        }
    };

    /**
     * Null-safe trim.
     *
     * @param s The String to trim.
     * @return A string with.
     */
    public static String trim( String s ) {
        return s != null ? s.trim() : null;
    }

    /**
     * Trim trailing spaces.
     *
     * @param s The String to trim.
     * @return a copy of the string with the trailing spaces removed.
     */
    public static String rtrim( String s ) {
        if ( s != null ) {
            int initialPos = s.length() - 1;
            int i = initialPos;
            while ( i >= 0 ) {
                if ( Character.isWhitespace( s.charAt( i ) ) ) i--;
                else break;
            }
            if ( i == initialPos ) {
                return s;
            } else {
                return s.substring( 0, i + 1 );
            }
        } else {
            return null;
        }
    }

    /**
     * @return a copy of the string with the leading spaces removed.
     */
    public static String ltrim( String s ) {
        if ( s != null ) {
            int i = 0;
            while ( i < s.length() ) {
                if ( Character.isWhitespace( s.charAt( i ) ) ) i++;
                else break;
            }
            if ( i == 0 ) {
                return s;
            } else {
                return s.substring( i, s.length() );
            }
        } else {
            return null;
        }
    }

    /**
     * Verify that the string contains all blank characters or is null.
     * <p/>
     * <code>
     * Examples of blank strings :
     * "", "     ", null
     * </code>
     *
     * @return true if string is blank.
     */
    public static boolean isBlank( CharSequence s ) {
        if ( s != null ) {
            int i = 0;
            while ( i < s.length() ) if ( !Character.isWhitespace( s.charAt( i ) ) ) {
                return false;
            } else i++;
        }
        return true;
    }

    /**
     * Apply a transform that will replace blank values with null.
     */
    public static String blankToNull( String s ) {
        return s == null || isBlank( s ) ? null : s;
    }

    /**
     * Apply a transform that will replace null values with blank.
     */
    public static String nullToBlank( String s ) {
        return s == null ? "" : s;
    }

    /**
     * Apply a constraint over string length.
     */
    public static String maxLength( String s, int length ) {
        if ( s == null ) {
            return null;
        } else if ( s.length() <= length ) {
            return s;
        } else {
            throw new DataTruncationException();
        }
    }

    /**
     * Apply a lowercase transform to the string.
     *
     * @param locale The locale to use in the transformation,
     * @return A lowercase string.
     */
    public static String lowercase( String s, Locale locale ) {
        return s != null ? s.toLowerCase( locale ) : null;
    }

    /**
     * Apply an uppercase transform to the string.
     *
     * @param locale The locale to use in the transformation,
     * @return A uppercase string.
     */
    public static String uppercase( String s, Locale locale ) {
        return s != null ? s.toUpperCase( locale ) : null;
    }
}
