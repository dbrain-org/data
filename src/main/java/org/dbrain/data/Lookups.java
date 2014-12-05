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

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by epoitras on 03/06/14.
 */
public class Lookups {

    /**
     * Build a lookup table, providing initial values.
     *
     * @param fromValue initial from value.
     * @param toValue   initial to value.
     * @return A Builder to continue building a lookup.
     */
    public static <FROM, TO> LookupBuilder<FROM, TO> map( FROM fromValue, TO toValue ) {
        LookupBuilder<FROM, TO> result = new LookupBuilder<>();
        return result.map( fromValue, toValue );
    }

    /**
     * Inverse an existing lookup table. Please note that fails if multiple from values map to the same to value.
     *
     * @param lookup The lookup to inverse.
     * @return An inverse lookup.
     */
    public static <FROM, TO> Lookup<TO, FROM> inverse( Lookup<FROM, TO> lookup ) {
        LookupBuilder<TO, FROM> result = new LookupBuilder<TO, FROM>().preventNullMapping();
        for ( FROM fromValue : lookup.keySet() ) {
            TO toValue = lookup.apply( fromValue );
            result.map( toValue, fromValue );
        }
        return result.build();
    }

    /**
     * Builder for Lookup.
     *
     * @param <FROM>
     * @param <TO>
     */
    public static class LookupBuilder<FROM, TO> {

        private boolean defaultNullMapping = true;

        private Map<FROM, TO> map = new HashMap<>();


        public LookupBuilder() {
        }

        /**
         * Map a value to another.
         */
        public LookupBuilder<FROM, TO> map( FROM fromValue, TO toValue ) {
            if ( map.containsKey( fromValue ) ) {
                throw new IllegalArgumentException();
            }

            // A null value has been manually assigned ?
            if ( fromValue == null ) {
                defaultNullMapping = false;
            }

            map.put( fromValue, toValue );
            return this;
        }

        /**
         * Prevent the default mapping of null to null.
         */
        public LookupBuilder<FROM, TO> preventNullMapping() {
            defaultNullMapping = false;
            return this;
        }

        /**
         * Build a lookup table.
         */
        public Lookup<FROM, TO> build() {
            try {
                if ( defaultNullMapping ) {
                    map.put( null, null );
                }
                return new LookupImpl<>( map );
            } finally {
                map = null;
            }
        }

        /**
         * Implementation of the lookup interface.
         */
        private static class LookupImpl<FROM, TO> implements Lookup<FROM, TO>, Serializable {

            private final Set<FROM>     possibleValues;
            private final Map<FROM, TO> map;

            private LookupImpl( Map<FROM, TO> map ) {
                this.map = map;
                this.possibleValues = Collections.unmodifiableSet( map.keySet() );
            }

            @Override
            public Set<FROM> keySet() {
                return possibleValues;
            }

            @Override
            public TO apply( FROM from ) {
                if ( map.containsKey( from ) ) {
                    return map.get( from );
                } else {
                    throw new IllegalArgumentException();
                }
            }
        }
    }
}
