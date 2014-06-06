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

package epic.data.util;

import java.util.function.Function;

/**
 * Classes that contains static methods to manipulate adapters.
 */
public class Functions {

    /**
     * Combine 2 adapters that process the same type. This method allows null since each adapter is interchangeable.
     *
     * @param function1 The adapter 1 to plug, or null.
     * @param function2 The adapter 2 to plug, or null.
     * @param <T>      The type these adapter processes.
     * @return
     */
    public static <T> Function<T, T> composeAlike( final Function<T, T> function1, final Function<T, T> function2 ) {
        if ( function1 == null ) {
            return function2;
        } else {
            if ( function2 == null ) {
                return function1;
            } else {
                return function1.compose( function2 );
            }
        }
    }

    public static <T> Function<T, T> composeAlike( final Function<T, T> function1, final Function<T, T> function2, final Function<T, T> function3 ) {
        return composeAlike( composeAlike( function1, function2 ), function3 );
    }

    public static <T> Function<T, T> composeAlike( final Function<T, T> function1, final Function<T, T> function2, final Function<T, T> function3, final Function<T, T> function4 ) {
        return composeAlike( composeAlike( composeAlike( function1, function2 ), function3 ), function4 );
    }

    /**
     * Plugs two adapters together to make a single combined adapter.
     *
     * @param function1 The adapter 1 to plug.
     * @param function2 The adapter 2 to plug.
     * @param <FROM>
     * @param <TO>                           combineAlike
     * @param <MIDDLE>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> TO
     */
    public static <FROM, TO, MIDDLE> Function<FROM, TO> compose( final Function<FROM, ? extends MIDDLE> function1, final Function<MIDDLE, TO> function2 ) {
        return new Function<FROM, TO>() {
            @Override
            public TO apply( FROM from ) {
                return function2.apply( function1.apply( from ) );
            }
        };
    }

    /**
     * Combine three adapters together to make a single adapter.
     *
     * @param function1  The adapter 1 to plug.
     * @param function2  The adapter 2 to plug.
     * @param function3  The adapter 23to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE1>
     * @param <MIDDLE2>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> Adapter3 --> TO
     */
    public static <FROM, TO, MIDDLE1, MIDDLE2> Function<FROM, TO> compose( Function<FROM, ? extends MIDDLE1> function1, Function<MIDDLE1, ? extends MIDDLE2> function2, Function<MIDDLE2, TO> function3 ) {
        return compose( compose( function1, function2 ), function3 );
    }

    /**
     * Combine four adapters together to make a single adapter.
     *
     * @param function1  The adapter 1 to plug.
     * @param function2  The adapter 2 to plug.
     * @param function3  The adapter 3 to plug.
     * @param function4  The adapter 3 to plug.
     * @param <FROM>
     * @param <TO>
     * @param <MIDDLE1>
     * @param <MIDDLE2>
     * @return A combined adapter doing: FROM --> Adapter1 --> Adapter2 --> Adapter3 --> Adapter4 --> TO
     */
    public static <FROM, TO, MIDDLE1, MIDDLE2, MIDDLE3> Function<FROM, TO> compose( Function<FROM, ? extends MIDDLE1> function1, Function<MIDDLE1, ? extends MIDDLE2> function2, Function<MIDDLE2, ? extends MIDDLE3> function3, Function<MIDDLE3, TO> function4 ) {
        return compose( compose( function1, function2 ), compose( function3, function4 ) );
    }


}
