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

import java.util.function.IntConsumer;

/**
 * Created by epoitras on 06/11/14.
 */
public class ParserUtils {

    // Skip consecutive white spaces
    public static void skipWhitespaces( ReaderCursor c ) {
        while ( c.is( Character::isWhitespace ) ) {
            c.next();
        }
    }

    // True if the character is a digit.
    public static boolean isDigit( int cur ) {
        return cur >= '0' && cur <= '9';
    }

    // Read a quoted attribute.
    public static String readQuotedString( ReaderCursor c ) {
        int quote = c.read();
        StringBuilder sb = new StringBuilder();
        do {
            int current = c.read();
            if ( current == quote ) {
                if ( c.current() == quote ) {
                    sb.appendCodePoint( c.read() );
                } else {
                    break;
                }
            } else if ( current < 0 ) {
                throw c.error( "Unexpected eof" );
            } else {
                sb.appendCodePoint( current );
            }
        } while ( true );
        return sb.toString();
    }

    public static boolean isJavaIdentifier( String s ) {
        if ( s == null || s.length() == 0 ) {
            return false;
        }
        if ( !Character.isJavaIdentifierStart( s.codePointAt( 0 ) ) ) {
            return false;
        }
        return s.substring( 1 ).codePoints().allMatch( Character::isJavaIdentifierPart );
    }

    // Read an unquoted segment.
    public static String readJavaIdentifier( ReaderCursor c ) {
        if ( c.is( Character::isJavaIdentifierStart ) ) {
            StringBuilder sb = new StringBuilder();
            sb.appendCodePoint( c.read() );
            for ( int cur = c.current(); Character.isJavaIdentifierPart( cur ); cur = c.next() ) {
                sb.appendCodePoint( cur );
            }
            return sb.toString();
        } else {
            return null;
        }
    }
}
