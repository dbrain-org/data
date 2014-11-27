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

package org.dbrain.data.impl.fqn;

import org.dbrain.data.Fqn;
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by epoitras on 25/11/14.
 */
public class FqnUtils {
    private static final String RESERVED_CHARS = "*\'\"?!@#%&()[]{}.,;+-/\\^ ";
    static final         Fqn    NULL_VALUE     = new FqnImpl( null );

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Fqn parseFqn( ReaderCursor c ) {

        // Skip whitespace
        skipWhitespace( c );

        // Parse the name
        if ( isFqnStart( c.get() ) ) {
            List<String> segments = new ArrayList<>();
            segments.add( readSegment( c ) );
            while ( c.get() == '.' ) {
                c.read();
                segments.add( readSegment( c ) );
            }
            return new FqnImpl( segments );
        }
        return NULL_VALUE;
    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static Fqn parseFqn( String fqn ) {
        if ( fqn == null ) {
            return NULL_VALUE;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        Fqn result = Fqn.of( c );

        // Skip trailing whitespace
        skipWhitespace( c );

        // Expect EOF
        if ( c.get() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // Skip consecutive white spaces
    private static void skipWhitespace( ReaderCursor c ) {
        while ( ParserUtils.isSpace( c.get() )) {
            c.read();
        }
    }

    // True if the character is a reserved one and therefore cannot be in a unquoted segment.
    private static boolean isNotReserved( int cur ) {
        return RESERVED_CHARS.indexOf( cur ) < 0;
    }

    // True if the characted is a quote.
    private static boolean isQuote( int cur ) {
        return cur == '\'';
    }

    // True if the character is a possible fully qualified name start.
    private static boolean isFqnStart( int cur ) {
        return cur >= 0 && ( isQuote( cur ) || isNotReserved( cur ) );
    }

    // True if the character is a unquoted segment character.
    private static boolean isUnquotedSegment( int cur ) {
        return cur >= 0 && isNotReserved( cur );
    }

    // Read a quoted segment.
    private static String readQuotedSegment( ReaderCursor c ) {
        int quote = c.read();
        StringBuilder sb = new StringBuilder();
        do {
            int current = c.read();
            if ( current == quote ) {
                if ( c.get() == quote ) {
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

    // Read an unquoted segment.
    private static String readUnquotedSegment( ReaderCursor cursor ) {
        StringBuilder sb = new StringBuilder();
        while ( isUnquotedSegment( cursor.get() ) ) {
            sb.appendCodePoint( cursor.read() );
        }
        return sb.toString();
    }

    // Read a segment.
    private static String readSegment( ReaderCursor c ) {
        if ( isQuote( c.get() ) ) {
            return readQuotedSegment( c );
        } else {
            return readUnquotedSegment( c );
        }
    }

    /**
     * Encode a segment of a Fully Qualified Name.
     */
    static String encodeSegment( String segment ) {
        if ( segment.length() == 0 ) {
            return "''";
        }
        StringBuilder sb = null;
        for ( int i = 0; i < segment.length(); i++ ) {
            Character c = segment.charAt( i );
            if ( RESERVED_CHARS.indexOf( c ) >= 0 ) {
                if ( sb == null ) {
                    sb = new StringBuilder( segment.length() + 12 );
                    sb.append( "'" );
                    sb.append( segment.substring( 0, i ) );
                }
                if ( c == '\'' ) {
                    sb.append( "''" );
                } else {
                    sb.append( c );
                }
            } else {
                if ( sb != null ) {
                    sb.append( c );
                }
            }
        }

        // Close the string as we escaped it.
        if ( sb != null ) {
            sb.append( "\'" );
            return sb.toString();
        } else {
            return segment;
        }
    }
}
