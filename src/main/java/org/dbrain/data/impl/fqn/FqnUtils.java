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
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements parsing of Fully Qualified Name and Patterns.
 */
public final class FqnUtils {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Fqn parseFqn( ReaderCursor c ) {

        // Skip whitespace
        ParserUtils.skipWhitespaces( c );

        // Parse the name
        if ( c.is( Character::isJavaIdentifierStart ) || c.is( "'") ) {
            List<String> segments = new ArrayList<>();
            segments.add( readSegment( c ) );
            while ( c.is( "." ) ) {
                c.next();
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
        ParserUtils.skipWhitespaces( c );

        // Expect EOF
        if ( !c.eof() ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    /**
     * Create a fully qualified name pattern from a ReaderCursor.
     */
    public static FqnPattern parseFqnPattern( ReaderCursor c ) {

        // Skip whitespace
        ParserUtils.skipWhitespaces( c );

        // Parse the name
        if ( c.is( "'*" ) || c.is( Character::isJavaIdentifierStart ) ) {
            FqnPatternBuilderImpl builder = new FqnPatternBuilderImpl();
            readPatternSegment( c, builder );
            while ( c.is( "." ) ) {
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
        ParserUtils.skipWhitespaces( c );

        // Expect EOF
        if ( !c.eof() ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
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

    // Read a segment.
    private static String readSegment( ReaderCursor c ) {
        if ( c.is( "\'" ) ) {
            return ParserUtils.readQuotedString( c );
        } else {
            return ParserUtils.readJavaIdentifier( c );
        }
    }

    // Read a wildcard segment
    private static void readWildcardSegment( ReaderCursor c, FqnPattern.Builder to ) {
        c.skip( "*" );
        if ( c.is( "*" ) ) {
            to.any();
            c.next();
        } else {
            to.one();
        }
    }

    // Read a segment.
    private static void readPatternSegment( ReaderCursor c, FqnPattern.Builder to ) {
        if ( c.is( "'" ) ) {
            to.segment( ParserUtils.readQuotedString( c ) );
        } else if ( c.is( "*" ) ) {
            readWildcardSegment( c, to );
        } else if ( c.is( Character::isJavaIdentifierStart ) ) {
            to.segment( ParserUtils.readJavaIdentifier( c ) );
        } else {
            throw c.error( "Expected Fqn pattern segment" );
        }
    }

    /**
     * Encode a segment of a Fully Qualified Name.
     */
    static String encodeSegment( String segment ) {
        if ( segment.length() == 0 ) {
            return "''";
        }
        if ( ParserUtils.isJavaIdentifier( segment ) ) {
            return segment;
        } else {
            return "'" + segment.replaceAll( "'", "''" ) + "'";
        }
    }
}
