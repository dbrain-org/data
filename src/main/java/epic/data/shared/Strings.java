/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package epic.data.shared;

import java.util.Comparator;

/**
 * Contains static methods to manipulate strings.
 */
public class Strings {

    public static final Comparator<String> NATURAL_ORDER = new Comparator<String>() {
        @Override
        public int compare( String o1, String o2 ) {
            if ( o1 == null ) {
                return o2 == null ? 0 : -1;
            } else {
                return o2 == null ? 1 : o1.compareTo( o2 );
            }
        }
    };


    /**
     * Trim trailing spaces.
     *
     * @param s The String to trim.
     * @return a copy of the string with the trailing spaces removed.
     */
    public static String rtrim( String s ) {
        int initialPos = s.length() - 1;
        if ( s != null ) {
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
        boolean retval = true;
        if ( s != null ) {
            int i = 0;
            while ( i < s.length() ) if ( !Character.isWhitespace( s.charAt( i ) ) ) {
                retval = false;
                break;
            } else i++;
        }
        return retval;
    }

}
