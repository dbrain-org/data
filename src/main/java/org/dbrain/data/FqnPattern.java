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
     * Match a pattern against a name.
     *
     * @return The match result.
     */
    MatchResult match( Fqn fqn );

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
     * Allows to build Fqn Patterns.
     */
    interface Builder {



        Builder segment( String segment );

        Builder one();

        Builder many();

        FqnPattern build();

    }
}
