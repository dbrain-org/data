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

package org.dbrain.data.impl.fqn;

import org.dbrain.data.FqnPattern;
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

/**
 * Created by epoitras on 9/4/15.
 */
public class FqnPatternParseUtils {
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
}
