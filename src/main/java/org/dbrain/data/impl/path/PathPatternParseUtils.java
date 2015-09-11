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
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;

/**
 * Created by epoitras on 9/3/15.
 */
public class PathPatternParseUtils {

    // Read a wildcard segment
    private static void readWildcardNode( ReaderCursor c, PathPattern.Builder to ) {
        c.skip( "*" );
        if ( c.is( "*" ) ) {
            to.any();
            c.next();
        } else {
            to.one();
        }
    }

    // Read a segment.
    private static void readPatternNode( ReaderCursor c, PathPattern.Builder to ) {
        if ( c.is( Character::isJavaIdentifierStart ) ) {
            to.attr( PathParseUtils.readAttribute( c ) );
        } else if ( c.is( "[" ) ) {
            readBracketContent( c, to );
        } else if ( c.is( "*" ) ) {
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
        ParserUtils.skipWhitespaces( c );

        // Parse the name
        if ( c.is( Character::isJavaIdentifierStart ) || c.is( "[*" ) ) {
            PathPatternBuilderImpl builder = new PathPatternBuilderImpl();
            readPatternNode( c, builder );
            while ( c.is( "[." ) ) {
                if ( c.is( "." ) ) {
                    c.next();
                }
                readPatternNode( c, builder );
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
        ParserUtils.skipWhitespaces( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // Read an index.
    private static void readBracketContent( ReaderCursor c, PathPattern.Builder to ) {
        c.skip( "[" ); // Skip start

        // skip leading whitespaces
        ParserUtils.skipWhitespaces( c );

        if ( c.is( ParserUtils::isDigit ) ) {
            to.index( PathParseUtils.readLong( c ) );
        } else if ( c.is( "\'" ) ) {
            to.attr( ParserUtils.readQuotedString( c ) );
        } else {
            throw c.error( "Expecting quoted string or numerical index" );
        }

        // skip trailing whitespaces
        ParserUtils.skipWhitespaces( c );

        c.skip( "]" );
    }
}
