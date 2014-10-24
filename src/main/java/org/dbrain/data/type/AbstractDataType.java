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

package org.dbrain.data.type;

/**
 * Abstract base class to implements data types.
 */
public abstract class AbstractDataType<T> implements DataType<T> {

    @Override
    public final int compare( Object o1, Object o2 ) {
        if ( o1 == o2 ) {
            return 0;
        }
        return getComparator().compare( cast( o1 ), cast( o2 ) );
    }

    @Override
    public final boolean equals( Object value1, Object value2 ) {
        if ( value1 == value2 ) {
            return true;
        }
        return getComparator().compare( cast( value1 ), cast( value2 ) ) == 0;
    }

    @Override
    public final boolean in( Object o1, Object... vn ) {
        T v1 = cast( o1 );
        for ( Object value : vn ) {
            if ( getComparator().compare( v1, cast( value ) ) == 0 ) {
                return true;
            }
        }
        return false;
    }

    @Override
    public final T cast( Object value ) {
        return getCastFunction().apply( value );
    }

    @Override
    public String toString( Object value ) {
        return getDisplayFormatter().format( cast( value ) );
    }
}
