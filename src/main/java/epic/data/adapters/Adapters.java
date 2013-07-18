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

/**
 * Classes that contains static methods to manipulate adapters.
 */
public class Adapters {

    public static <T> Adapter<T, T> combineAlike( final Adapter<T, T> adapter1, final Adapter<T, T> adapter2 ) {
        if (adapter1 == null) {
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
            public TO adapt( FROM from ) {
                return adapter2.adapt( adapter1.adapt( from ) );
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

}
