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

package org.dbrain.data;

import org.dbrain.data.impl.path.PathPatternBuilderImpl;
import org.dbrain.data.impl.path.PathPatternParser;

/**
 * Pattern to match a Path.
 */
public interface PathPattern {

    /**
     * @return A new builder instance.
     */
    static Builder newBuilder() {
        return new PathPatternBuilderImpl();
    }

    /**
     * Create a new Path Pattern from a String.
     * Expect to works with the output of toString.
     */
    static PathPattern of( String path ) {
        return PathPatternParser.parsePathPattern( path );
    }

    /**
     * Match a pattern against a name.
     *
     * @return The match result.
     */
    MatchResult match( Path path );

    /**
     * @return The specifications of this pattern.
     */
    Specs getSpecs();

    /**
     * Define the type of pattern.
     */
    enum Type {

        /**
         * The pattern define an exact match.
         */
        EXACT_MATCH,

        /**
         *
         */
        PARTIAL

    }

    /**
     * Match result of a pattern matching.
     */
    interface MatchResult {

        /**
         * @return true if the pattern matched.
         */
        boolean matched();

        int partCount();

        Path getPart( int idx );

    }

    /**
     * Specifications of this pattern.
     */
    interface Specs {

        /**
         * @return The type of pattern.
         */
        Type getType();

        /**
         * Return the contextual name of this pattern. This value is the leading static nodes
         * of this pattern, if any.
         */
        Path scope();


    }

    /**
     * Allows to build Path Patterns.
     */
    interface Builder {

        /**
         * Match a specific attribute.
         */
        Builder attr( String attr );

        /**
         * Match a specific index.
         */
        Builder index( long index );

        /**
         * Match exactly one node, capture a part.
         */
        Builder one();

        /**
         * Match 0 to n nodes, caturing a part.
         */
        Builder any();

        /**
         * Build the pattern.
         */
        PathPattern build();

    }
}
