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
import org.dbrain.data.FqnPattern;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements parsing of Fully Qualified Name and Patterns.
 */
public final class FqnUtils {

    // Fqn reserved characters
    public static final String FQN_PATTERN_RESERVED_CHARS = "\'\"?!@#%&()[]{}.,;+-/\\^ ";

    // Fqn reserved characters
    public static final String FQN_RESERVED_CHARS = "*" + FQN_PATTERN_RESERVED_CHARS;

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Fqn parseFqn( ReaderCursor c ) {

        // Skip whitespace
        skipWhitespace( c );

        // Parse the name
        if ( isFqnStart( c.current() ) ) {
            List<String> segments = new ArrayList<>();
            segments.add( readSegment( c ) );
            while ( c.current() == '.' ) {
                c.read();
                segments.add( readSegment( c ) );
            }
            return new FqnImpl( segments );
        }
        return FqnImpl.EMPTY_NAME;
    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static Fqn parseFqn( String fqn ) {
        if ( fqn == null ) {
            return FqnImpl.EMPTY_NAME;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        Fqn result = parseFqn( c );

        // Skip trailing whitespace
        skipWhitespace( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    /**
     * Create a fully qualified name pattern from a ReaderCursor.
     */
    public static FqnPattern parseFqnPattern( ReaderCursor c ) {

        // Skip whitespace
        skipWhitespace( c );

        // Parse the name
        if ( isFqnPatternStart( c.current() ) ) {
            FqnPatternBuilderImpl builder = new FqnPatternBuilderImpl();
            readPatternSegment( c, builder );
            while ( c.current() == '.' ) {
                c.read();
                readPatternSegment( c, builder );
            }
            return builder.build();
        }
        return new FqnPatternBuilderImpl().build();

    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static FqnPattern parseFqnPattern( String fqn ) {
        if ( fqn == null ) {
            return FqnPatternImpl.EMPTY_PATTERN;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        FqnPattern result = parseFqnPattern( c );

        // Skip trailing whitespace
        skipWhitespace( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }


    // Skip consecutive white spaces
    private static void skipWhitespace( ReaderCursor c ) {
        while ( c.is( Character::isWhitespace ) ) {
            c.read();
        }
    }

    // True if the character is a reserved one and therefore cannot be in a unquoted segment.
    private static boolean isNotFqnReserved( int cur ) {
        return FQN_RESERVED_CHARS.indexOf( cur ) < 0;
    }

    // True if the character is a reserved one and therefore cannot be in a unquoted segment.
    private static boolean isNotFqnPatternReserved( int cur ) {
        return FQN_PATTERN_RESERVED_CHARS.indexOf( cur ) < 0;
    }

    // True if the character is a quote.
    private static boolean isQuote( int cur ) {
        return cur == '\'';
    }

    // True if the current character is a wildcard
    private static boolean isWildcard( int cur ) {
        return cur == '*';
    }

    // True if the character is a possible fully qualified name start.
    private static boolean isFqnStart( int cur ) {
        return cur >= 0 && ( isQuote( cur ) || isNotFqnReserved( cur ) );
    }

    // True if the character is a possible fully qualified name start.
    private static boolean isFqnPatternStart( int cur ) {
        return cur >= 0 && ( isQuote( cur ) || isNotFqnPatternReserved( cur ) );
    }


    // True if the character is a unquoted segment character.
    private static boolean isUnquotedSegment( int cur ) {
        return cur >= 0 && isNotFqnReserved( cur );
    }

    // Read a quoted segment.
    private static String readQuotedSegment( ReaderCursor c ) {
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

    // Read an unquoted segment.
    private static String readUnquotedSegment( ReaderCursor cursor ) {
        StringBuilder sb = new StringBuilder();
        while ( isUnquotedSegment( cursor.current() ) ) {
            sb.appendCodePoint( cursor.read() );
        }
        return sb.toString();
    }

    // Read a segment.
    private static String readSegment( ReaderCursor c ) {
        if ( isQuote( c.current() ) ) {
            return readQuotedSegment( c );
        } else {
            return readUnquotedSegment( c );
        }
    }

    // Read a wildcard segment
    private static void readWildcardSegment( ReaderCursor c, FqnPattern.Builder to ) {
        if ( isWildcard( c.next() ) ) {
            to.any();
            c.read();
        } else {
            to.one();
        }
    }

    // Read a segment.
    private static void readPatternSegment( ReaderCursor c, FqnPattern.Builder to ) {
        int cur = c.current();
        if ( isQuote( cur ) ) {
            to.segment( readQuotedSegment( c ) );
        } else if ( isWildcard( cur ) ) {
            readWildcardSegment( c, to );
        } else {
            to.segment( readUnquotedSegment( c ) );
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
            if ( FQN_RESERVED_CHARS.indexOf( c ) >= 0 ) {
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
