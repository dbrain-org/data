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

package epic.data.adapters;

import epic.data.Adapter;
import epic.data.shared.Strings;

import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 20/03/13
 * Time: 5:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class StringAdapters {

    /**
     * Apply a transform that will replace blank values with null.
     */
    public static Adapter<String, String> BLANK_TO_NULL = new Adapter<String, String>() {
        @Override
        public String adapt( String s ) {
            return s == null || Strings.isBlank( s ) ? null : s;
        }
    };

    /**
     * Apply a transform that will replace null values with blank.
     */
    public static Adapter<String, String> NULL_TO_BLANK = new Adapter<String, String>() {
        @Override
        public String adapt( String s ) {
            return s == null ? "" : s;
        }
    };


    /**
     * Apply a transform on the String that remove both leading and trailing blank spaces.
     */
    public static Adapter<String, String> TRIM = new Adapter<String, String>() {
        @Override
        public String adapt( String s ) {
            if ( s == null ) {
                return null;
            } else {
                return s.trim();
            }
        }
    };

    /**
     * Apply a transform on the String that remove trailing blank spaces.
     */
    public static Adapter<String, String> RTRIM = new Adapter<String, String>() {
        @Override
        public String adapt( String s ) {
            if ( s == null ) {
                return null;
            } else {
                return Strings.rtrim( s );
            }
        }
    };

    /**
     * Apply a transform on the String that remove both leading blank spaces.
     */
    public static Adapter<String, String> LTRIM = new Adapter<String, String>() {
        @Override
        public String adapt( String s ) {
            if ( s == null ) {
                return null;
            } else {
                return Strings.ltrim( s );
            }
        }
    };

    /**
     * Apply a constraint over string length.
     */
    public static Adapter<String, String> maxLength( final int length ) {
        return new Adapter<String, String>() {
            @Override
            public String adapt( String s ) {
                if ( s == null ) {
                    return null;
                } else if ( s.length() <= length ) {
                    return s;
                } else {
                    throw new DataTruncationException();
                }
            }
        };
    }

    /**
     * Apply a lowercase transform to the string.
     * @param locale The locale to use in the transformation,
     * @return A lowercase string.
     */
    public static Adapter<String, String> lowercase( final Locale locale ) {
        return new Adapter<String, String>() {
            @Override
            public String adapt( String s ) {
                return s != null ? s.toLowerCase( locale ) : null;
            }
        };
    }

    /**
     * Apply an uppercase transform to the string.
     * @param locale The locale to use in the transformation,
     * @return A uppercase string.
     */
    public static Adapter<String, String> uppercase( final Locale locale ) {
        return new Adapter<String, String>() {
            @Override
            public String adapt( String s ) {
                return s != null ? s.toUpperCase( locale ) : null;
            }
        };
    }

}
