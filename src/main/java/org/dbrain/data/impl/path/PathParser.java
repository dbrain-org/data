/*
 * Copyright [2015] [Eric Poitras]
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

package org.dbrain.data.impl.path;

import org.dbrain.data.Path;
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;

/**
 * Implements parsing of Paths.
 */
public final class PathParser {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Path parsePath( ReaderCursor c ) {

        // Skip whitespace
        ParserUtils.skipWhitespaces( c );

        // Parse the name
        if ( c.is( Character::isJavaIdentifierStart ) || c.is( "[" ) ) {
            Path.Builder builder = Path.newBuilder();
            readPathNode( c, builder );
            while ( true ) {
                if ( c.is( "[" ) ) {
                    readIndex( c, builder );
                } else if ( c.is( "." ) ) {
                    c.next(); // Skip accessor
                    builder.attr( readAttribute( c ) );
                } else {
                    break;
                }
            }
            return builder.build();
        }
        return PathImpl.EMPTY_PATH;
    }

    /**
     * Create a new Path from a String. Compatible with toString.
     */
    public static Path parsePath( String path ) {
        if ( path == null ) {
            return PathImpl.EMPTY_PATH;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( path ) );

        // Parse the name
        Path result = parsePath( c );

        // Skip trailing whitespace
        ParserUtils.skipWhitespaces( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // Read an attribute.
    public static String readAttribute( ReaderCursor c ) {
        // Should be called where current = isAttributeStart
        StringBuilder sb = new StringBuilder();
        for ( int cur = c.current(); Character.isJavaIdentifierPart( cur ); cur = c.next() ) {
            sb.appendCodePoint( cur );
        }
        return sb.toString();
    }

    public static long readLong( ReaderCursor c ) {
        long index = 0;
        for ( int cur = c.current(); ParserUtils.isDigit( cur ); cur = c.next() ) {
            index = index * 10 + cur - '0';
        }
        return index;
    }

    // Read an index.
    private static void readIndex( ReaderCursor c, Path.Builder to ) {
        c.next(); // Skip Index start

        // skip leading whitespaces
        ParserUtils.skipWhitespaces( c );

        if ( c.is( ParserUtils::isDigit ) ) {
            to.index( readLong( c ) );
        } else if ( c.is( "\'" ) ) {
            to.attr( ParserUtils.readQuotedString( c ) );
        } else {
            throw c.error( "Expecting quoted string or numerical index" );
        }

        // skip trailing whitespaces
        ParserUtils.skipWhitespaces( c );

        if ( c.is( "]" ) ) {
            c.next();
        } else {
            throw c.error( "Expecting ]" );
        }
    }

    // Read a node.
    private static void readPathNode( ReaderCursor c, Path.Builder to ) {
        if ( c.is( "[" ) ) {
            readIndex( c, to );
        } else if ( c.is( Character::isJavaIdentifierStart ) ) {
            to.attr( readAttribute( c ) );
        } else {
            throw c.error( "Expected attribute or [" );
        }
    }

    /**
     * Encode a path attribute.
     */
    static String encodeAttribute( String attr, boolean first ) {
        if ( attr.length() == 0 ) {
            return "['']";
        }

        // We will allocate the string builder only if we escape the attribute.
        StringBuilder sb = null;
        for ( int i = 0; i < attr.length(); i++ ) {
            Character c = attr.charAt( i );
            if ( ( i > 0 && Character.isJavaIdentifierPart( c ) ) || ( i == 0 && Character.isJavaIdentifierStart( c ) ) ) {
                if ( sb != null ) {
                    sb.append( c );
                }
            } else {
                if ( sb == null ) {
                    sb = new StringBuilder( attr.length() + 12 );
                    sb.append( "['" );
                    sb.append( attr.substring( 0, i ) );
                }
                if ( c == '\'' ) {
                    sb.append( "''" );
                } else {
                    sb.append( c );
                }
            }
        }

        // Close the string as we escaped it.
        if ( sb != null ) {
            sb.append( "\']" );
            return sb.toString();
        } else {
            return first ? attr : "." + attr;
        }
    }
}
