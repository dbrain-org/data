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

/**
 * Created by epoitras on 06/11/14.
 */
public class ParserUtils {

    /**
     * The character represent a end of file as returned by cursor.
     */
    public static boolean isEOF( int c ) {
        return c < 0;
    }


    /**
     * Return true if the specified character is an end of line character.
     */
    public static boolean isEOL( int c ) {
        return c == 13 || c == 10 || isEOF( c );
    }

    /**
     * Return true if the specified character is a space character.
     */
    public static boolean isSpace( int c ) {
        return c >= 0 && c <= ' ' && !isEOL( c );
    }
}
