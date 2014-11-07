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

package org.dbrain.data;

import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Describe a fully qualified name.
 *
 * Syntax allows for wildcards as well as ways of escaping them.
 *
 * ''
 * test
 * test.''
 * test.123.'*'
 * test.123.'**'
 * test.123.'test*'
 */
public class Fqn {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Fqn of( ReaderCursor c ) {
        if ( c.is( Fqn::isFqnStart ) ) {
            List<String> segments = new ArrayList<>();
            segments.add( readSegment( c ) );
            while ( c.current() == '.' ) {
                c.discard();
                segments.add( readSegment( c ) );
            }
            return new Fqn( segments );
        }
        return null;
    }

    /**
     * Create a new Fully Qualified Name from a String. Works with toString output.
     */
    public static Fqn of( String fqn ) {
        if ( fqn == null ) {
            return null;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );
        while ( c.is( ParserUtils::isSpace ) ) {
            c.discard();
        }
        Fqn result = of( c );
        while ( c.is( ParserUtils::isSpace ) ) {
            c.discard();
        }
        if ( !c.is( ParserUtils::isEOF ) ) {
            throw c.error( "Expecting end of string." );
        }

        return result;
    }

    private static final String RESERVED_CHARS = "*\'\"?!@#%&()[]{}.,;+-/\\^ ";

    // True if the character is a reserved one and therefore cannot be in a unquoted segment.
    private static boolean isReserved( int cur ) {
        return RESERVED_CHARS.indexOf( cur ) >= 0;
    }

    // True if the characted is a quote.
    private static boolean isQuote( int cur ) {
        return cur == '\'';
    }

    // True if the character is a possible fully qualified name start.
    private static boolean isFqnStart( int cur ) {
        return cur >= 0 && ( isQuote( cur ) || !isReserved( cur ) );
    }

    // True if the character is a unquoted segment character.
    private static boolean isUnquotedSegment( int cur ) {
        return cur >= 0 && !isReserved( cur );
    }

    // Read a quoted segment.
    private static String readQuotedSegment( ReaderCursor c ) {
        int quote = c.current();
        StringBuilder sb = new StringBuilder();
        do {
            int current = c.next();
            if ( current == quote ) {
                current = c.next();
                if ( current == quote ) {
                    sb.appendCodePoint( quote );
                } else {
                    break;
                }
            } else if ( current < 0 ) {
                throw c.error( "Unexpected end of stream." );
            } else {
                sb.appendCodePoint( current );
            }
        } while ( true );
        return sb.toString();
    }

    // Read an unquoted segment.
    private static String readUnquotedSegment( ReaderCursor cursor ) {
        StringBuilder sb = new StringBuilder();
        while ( cursor.is( Fqn::isUnquotedSegment ) ) {
            sb.appendCodePoint( cursor.current() );
            cursor.discard();
        }
        return sb.toString();
    }

    // Read a segment.
    private static String readSegment( ReaderCursor c ) {
        if ( c.is( Fqn::isQuote ) ) {
            return readQuotedSegment( c );
        } else {
            return readUnquotedSegment( c );
        }
    }

    /**
     * Encode a segment of a Fully Qualified Name.
     */
    private static String encodeSegment( String segment ) {
        if ( segment.length() == 0 ) {
            return "''";
        }
        ;
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

    private final List<String> segments;

    private Fqn( List<String> segments ) {
        this.segments = segments != null && segments.size() > 0 ? new ArrayList<>( segments ) : null;
    }

    /**
     * @return The number of segments in this FQN.
     */
    public int getSize() {
        return segments != null ? segments.size() : 0;
    }

    /**
     * Retrieve the unencoded segment.
     *
     * @param i Index of the segment to retrieve.
     * @return A segment.
     */
    public String getSegment( int i ) {
        if ( segments != null ) {
            return segments.get( i );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * @return The string representation of this FQN.
     */
    public String toString() {
        if ( segments == null || segments.size() == 0 ) {
            return "";
        } else {
            if ( segments.size() == 1 ) {
                return encodeSegment( segments.get( 0 ) );
            } else {
                StringBuilder sb = null;
                for ( String s : segments ) {
                    if ( sb == null ) {
                        sb = new StringBuilder();
                    } else {
                        sb.append( "." );
                    }
                    sb.append( encodeSegment( s ) );
                }
                return sb.toString();
            }
        }
    }

}
