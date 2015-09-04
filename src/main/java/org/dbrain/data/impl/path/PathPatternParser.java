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

import org.dbrain.data.PathPattern;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;

/**
 * Created by epoitras on 9/3/15.
 */
public class PathPatternParser {

    // Read a wildcard segment
    private static void readWildcardNode( ReaderCursor c, PathPattern.Builder to ) {
        if ( isWildcard( c.next() ) ) {
            to.any();
            c.read();
        } else {
            to.one();
        }
    }

    // Read a segment.
    private static void readPatternNode( ReaderCursor c, PathPattern.Builder to ) {
        int cur = c.current();
        if ( PathParser.isAttributeStart( cur ) ) {
            to.attr( PathParser.readAttribute( c ) );
        } else if ( PathParser.isBracketOpen( cur ) ) {
            readBracketContent( c, to );
        } else if ( isWildcard( cur ) ) {
            readWildcardNode( c, to );
        } else {
            c.error( "Expecting an attribute or an index" );
        }
    }

    /**
     * Create a fully qualified name pattern from a ReaderCursor.
     */
    public static PathPattern parsePathPattern( ReaderCursor c ) {

        // Skip whitespace
        PathParser.skipWhitespace( c );

        // Parse the name
        if ( isPathPatternStart( c.current() ) ) {
            PathPatternBuilderImpl builder = new PathPatternBuilderImpl();
            readPatternNode( c, builder );
            int cur = c.current();
            while ( PathParser.isAttributeAccessor( cur ) || PathParser.isBracketOpen( cur ) ) {
                if ( PathParser.isAttributeAccessor( cur ) ) {
                    c.next();
                }
                readPatternNode( c, builder );
                cur = c.current();
            }
            return builder.build();
        }
        return new PathPatternBuilderImpl().build();

    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static PathPattern parsePathPattern( String fqn ) {
        if ( fqn == null ) {
            return PathPatternImpl.EMPTY_PATTERN;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        PathPattern result = parsePathPattern( c );

        // Skip trailing whitespace
        PathParser.skipWhitespace( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // True if the current character is a wildcard
    private static boolean isWildcard( int cur ) {
        return cur == '*';
    }

    // True if the character is a possible path pattern start.
    private static boolean isPathPatternStart( int cur ) {
        return PathParser.isPathStart( cur ) || isWildcard( cur );
    }

    // Read an index.
    private static void readBracketContent( ReaderCursor c, PathPattern.Builder to ) {
        c.next(); // Skip Index start

        // skip leading whitespaces
        PathParser.skipWhitespace( c );

        int cur = c.current();
        if ( PathParser.isDigit( cur ) ) {
            to.index( PathParser.readLong( c ) );
        } else if ( PathParser.isQuote( cur ) ) {
            to.attr( PathParser.readQuotedAttribute( c ) );
        } else {
            throw c.error( "Expecting quoted string or numerical index" );
        }

        // skip trailing whitespaces
        PathParser.skipWhitespace( c );

        if ( !PathParser.isBracketClose( c.read() ) ) {
            throw c.error( "Expecting ]" );
        }
    }
}
