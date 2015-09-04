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
public final class FqnParseUtils {

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
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static FqnPattern parseFqnPattern( String fqn ) {
        if ( fqn == null ) {
            return FqnPatternImpl.EMPTY_PATTERN;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        FqnPattern result = FqnPatternParseUtils.parseFqnPattern( c );

        // Skip trailing whitespace
        ParserUtils.skipWhitespaces( c );

        // Expect EOF
        if ( !c.eof() ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // Read a segment.
    private static String readSegment( ReaderCursor c ) {
        if ( c.is( "'" ) ) {
            return ParserUtils.readQuotedString( c );
        } else {
            return ParserUtils.readJavaIdentifier( c );
        }
    }

    /**
     * Encode a segment of a Fully Qualified Name.
     */
    public static String encodeSegment( String segment ) {
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
