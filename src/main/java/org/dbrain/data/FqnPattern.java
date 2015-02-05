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

import org.dbrain.data.impl.fqn.FqnPatternBuilderImpl;
import org.dbrain.data.impl.fqn.FqnUtils;

/**
 * Pattern to match a Fqn.
 */
public interface FqnPattern {

    /**
     * @return A new builder instance.
     */
    public static Builder newBuilder() {
        return new FqnPatternBuilderImpl();
    }

    /**
     * Create a new Fully Qualified Name Pattern from a String.
     * Expect to works with the output of toString.
     */
    static FqnPattern of( String fqn ) {
        return FqnUtils.parseFqnPattern( fqn );
    }

    /**
     * Match a pattern against a name.
     *
     * @return The match result.
     */
    MatchResult match( Fqn fqn );

    /**
     * @return The specifications of this pattern.
     */
    Specs getSpecs();

    /**
     * Match result of a pattern matching.
     */
    interface MatchResult {

        /**
         * @return true if the pattern matched.
         */
        boolean matched();

        int partCount();

        Fqn getPart( int idx );

    }

    /**
     * Define the type of pattern.
     */
    public enum Type {

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
        Fqn scope();


    }

    /**
     * Allows to build Fqn Patterns.
     */
    interface Builder {

        /**
         * Match a specific segment.
         */
        Builder segment( String segment );

        /**
         * Match exactly one segment, capture a part.
         */
        Builder one();

        /**
         * Match 0 to n segments, caturing a part.
         */
        Builder any();

        /**
         * Build the pattern.
         */
        FqnPattern build();

    }
}
