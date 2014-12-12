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

import org.dbrain.data.impl.fqn.FqnBuilderImpl;
import org.dbrain.data.impl.fqn.FqnImpl;
import org.dbrain.data.impl.fqn.FqnUtils;
import org.dbrain.data.text.ReaderCursor;

import java.util.Arrays;

/**
 * Describe a fully qualified name.
 * <p/>
 * Syntax allows for wildcards as well as ways of escaping them.
 * ''
 * test
 * test.''
 * test.123.'*'
 * test.123.'**'
 * test.123.'test*'
 */
public interface Fqn {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    static Fqn of( ReaderCursor c ) {
        return FqnUtils.parseFqn( c );
    }

    /**
     * Create a new Fully Qualified Name from a String.
     * Expect to works with the output of toString.
     */
    static Fqn of( String fqn ) {
        return FqnUtils.parseFqn( fqn );
    }

    static Fqn ofSegment( String segment ) {
        if ( segment != null ) {
            return new FqnImpl( Arrays.asList( segment ) );
        } else {
            return Fqn.of( (String) null );
        }
    }

    /**
     * Start building a Fqn from an initial segment.
     *
     * @return A builder.
     */
    static Builder fromSegment( String segment ) {
        return newBuilder().segment( segment );
    }

    /**
     * Start building a Fqn from another name.
     */
    static Builder from( Fqn fqn ) {
        return newBuilder().append( fqn );
    }

    /**
     * @return A new builder instance.
     */
    static Builder newBuilder() {
        return new FqnBuilderImpl();
    }

    /**
     * @return The number of segments in the name.
     */
    int size();

    /**
     * @return A single segment within the name.
     */
    String segment( int i );

    /**
     * @return Test if a name starts with another.
     */
    boolean startsWith( Fqn other );

    /**
     * Builder interface for the Fqn.
     */
    interface Builder {

        /**
         * Add a segment to the fqn.
         */
        Builder segment( String segment );

        /**
         * Append all segments of another fqn.
         */
        Builder append( Fqn fqn );

        /**
         * @return The built Fqn.
         * Fqn.of( "test" )
         */
        Fqn build();
    }

}
