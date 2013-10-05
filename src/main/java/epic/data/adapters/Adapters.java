/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package epic.data.adapters;

import epic.data.Adapter;
import epic.data.Lookup;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Classes that contains static methods to manipulate adapters.
 */
public class Adapters {

    /**
     * Combine 2 adapters that process the same type. This method allows null since each adapter is interchangeable.
     *
     * @param adapter1 The adapter 1 to plug, or null.
     * @param adapter2 The adapter 2 to plug, or null.
     * @param <T>      The type these adapter processes.
     * @return
     */
    public static <T> Adapter<T, T> combineAlike( final Adapter<T, T> adapter1, final Adapter<T, T> adapter2 ) {
        if ( adapter1 == null ) {
            return adapter2;
        } else {
            if ( adapter2 == null ) {
                return adapter1;
            } else {
                return combine( adapter1, adapter2 );
            }
        }
    }

    public static <T> Adapter<T, T> combineAlike( final Adapter<T, T> adapter1, final Adapter<T, T> adapter2, final Adapter<T, T> adapter3 ) {
        return combineAlike( combineAlike( adapter1, adapter2 ), adapter3 );
    }

    public static <T> Adapter<T, T> combineAlike( final Adapter<T, T> adapter1, final Adapter<T, T> adapter2, final Adapter<T, T> adapter3, final Adapter<T, T> adapter4 ) {
        return combineAlike( combineAlike( combineAlike( adapter1, adapter2 ), adapter3 ), adapter4 );
    }

    /**
     * Plugs two adapters together to make a single combined adapter.
     *
     * @param adapter1 The adapter 1 to plug.
     * @param adapter2 The adapter 2 to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> TO
     */
    public static <FROM, TO, MIDDLE> Adapter<FROM, TO> combine( final Adapter<FROM, ? extends MIDDLE> adapter1, final Adapter<MIDDLE, TO> adapter2 ) {
        return new Adapter<FROM, TO>() {
            @Override
            public TO apply( FROM from ) {
                return adapter2.apply( adapter1.apply( from ) );
            }
        };
    }

    /**
     * Combine three adapters together to make a single adapter.
     *
     * @param adapter1  The adapter 1 to plug.
     * @param adapter2  The adapter 2 to plug.
     * @param adapter3  The adapter 23to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE1>
     * @param <MIDDLE2>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> Adapter3 --> TO
     */
    public static <FROM, TO, MIDDLE1, MIDDLE2> Adapter<FROM, TO> combine( Adapter<FROM, ? extends MIDDLE1> adapter1, Adapter<MIDDLE1, ? extends MIDDLE2> adapter2, Adapter<MIDDLE2, TO> adapter3 ) {
        return combine( combine( adapter1, adapter2 ), adapter3 );
    }

    /**
     * Combine four adapters together to make a single adapter.
     *
     * @param adapter1  The adapter 1 to plug.
     * @param adapter2  The adapter 2 to plug.
     * @param adapter3  The adapter 3 to plug.
     * @param adapter4  The adapter 3 to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE1>
     * @param <MIDDLE2>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> Adapter3 --> Adapter4 --> TO
     */
    public static <FROM, TO, MIDDLE1, MIDDLE2, MIDDLE3> Adapter<FROM, TO> combine( Adapter<FROM, ? extends MIDDLE1> adapter1, Adapter<MIDDLE1, ? extends MIDDLE2> adapter2, Adapter<MIDDLE2, ? extends MIDDLE3> adapter3, Adapter<MIDDLE3, TO> adapter4 ) {
        return combine( combine( adapter1, adapter2 ), combine( adapter3, adapter4 ) );
    }

    /**
     * Combine five adapters together to make a single adapter.
     *
     * @param adapter1  The adapter 1 to plug.
     * @param adapter2  The adapter 2 to plug.
     * @param adapter3  The adapter 3 to plug.
     * @param adapter4  The adapter 4 to plug.
     * @param adapter5  The adapter 5 to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE1>
     * @param <MIDDLE2>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> Adapter3 --> Adapter4 --> Adapter5 --> TO
     */
    public static <FROM, TO, MIDDLE1, MIDDLE2, MIDDLE3, MIDDLE4> Adapter<FROM, TO> combine( Adapter<FROM, ? extends MIDDLE1> adapter1, Adapter<MIDDLE1, ? extends MIDDLE2> adapter2, Adapter<MIDDLE2, ? extends MIDDLE3> adapter3, Adapter<MIDDLE3, ? extends MIDDLE4> adapter4, Adapter<MIDDLE4, TO> adapter5 ) {
        return combine( combine( adapter1, adapter2 ), combine( combine( adapter3, adapter4 ), adapter5 ) );
    }

    /**
     * Build a lookup table, providing initial values.
     *
     * @param fromValue initial from value.
     * @param toValue   initial to value.
     * @return A Builder to continue building a lookup.
     */
    public static <FROM, TO> LookupBuilder<FROM, TO> lookupFrom( FROM fromValue, TO toValue ) {
        LookupBuilder<FROM, TO> result = new LookupBuilder<>();
        return result.map( fromValue, toValue );
    }

    /**
     * Inverse an existing lookup table. Please note that fails if multiple from values map to the same to value.
     *
     * @param lookup The lookup to inverse.
     * @return An inverse lookup.
     */
    public static <FROM, TO> Lookup<TO, FROM> inverseLookup( Lookup<FROM, TO> lookup ) {
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


        private LookupBuilder() {
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
    }

    /**
     * Implementation of the lookup interface.
     */
    private static class LookupImpl<FROM, TO> implements Lookup<FROM, TO> {

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
