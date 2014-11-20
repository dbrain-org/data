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

package org.dbrain.data.impl;

import org.dbrain.data.Fqn;
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements the Fqn.
 */
public final class FqnImpl implements Fqn {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Fqn of( ReaderCursor c ) {
        if ( FqnImpl.isFqnStart( c.get() ) ) {
            List<String> segments = new ArrayList<>();
            segments.add( FqnImpl.readSegment( c ) );
            while ( c.get() == '.' ) {
                c.read();
                segments.add( FqnImpl.readSegment( c ) );
            }
            return new FqnImpl( segments );
        }
        return FqnImpl.NULL_VALUE;
    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static Fqn of( String fqn ) {
        if ( fqn == null ) {
            return FqnImpl.NULL_VALUE;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );
        while ( ParserUtils.isSpace( c.get() ) ) {
            c.read();
        }
        Fqn result = of( c );
        while ( ParserUtils.isSpace( c.get() ) ) {
            c.read();
        }
        if ( c.get() >= 0 ) {
            throw c.error( "Expecting end of string" );
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

    private static final Fqn NULL_VALUE = new FqnImpl( null );

    private final List<String> segments;

    /**
     * Use the of(xxx) factory.
     */
    private FqnImpl( List<String> segments ) {
        this.segments = segments != null && segments.size() > 0 ? new ArrayList<>( segments ) : null;
    }

    /**
     * @return The number of segments in this FQN.
     */
    @Override
    public int size() {
        return segments != null ? segments.size() : 0;
    }

    /**
     * Retrieve the decoded segment.
     *
     * @param i Index of the segment to retrieve.
     * @return A segment.
     */
    @Override
    public String segment( int i ) {
        if ( segments != null ) {
            return segments.get( i );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * Validate that the current Fqn starts with the other Fqn.
     */
    @Override
    public boolean startsWith( Fqn other ) {
        if ( other == null || other == this ) {
            return true;
        }
        int otherSize = other.size();
        if ( size() < otherSize ) {
            return false;
        }
        for ( int i = 0; i < otherSize; i++ ) {
            if ( !segment( i ).equals( other.segment( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Fqn fqn = (Fqn) o;
        if ( size() != fqn.size() ) {
            return false;
        }
        for ( int i = 0; i < size(); i++ ) {
            if ( !segment( i ).equals( fqn.segment( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return segments.hashCode();
    }

    /**
     * @return The string representation of this FQN.
     */
    @Override
    public String toString() {
        final int size = size();
        if ( size == 0 ) {
            return "";
        } else {
            if ( size == 1 ) {
                return encodeSegment( segment( 0 ) );
            } else {
                StringBuilder sb = null;
                for ( int i = 0; i < size; i++ )
                {
                    String s = segment( i );
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
