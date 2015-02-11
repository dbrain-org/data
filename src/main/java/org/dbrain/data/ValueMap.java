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

import org.dbrain.data.access.NamedFieldAccessors;
import org.dbrain.data.impl.value.MapValueImpl;
import org.dbrain.data.impl.value.ValueMapBuilderImpl;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * An map of value.
 */
public interface ValueMap extends Value, java.util.Map<String, Value>, NamedFieldAccessors {

    /**
     * Create a new empty map.
     */
    static ValueMap newInstance() {
        return new MapValueImpl();
    }

    /**
     * Create a new builder.
     */
    static ValueMap.Builder newBuilder() {
        return new ValueMapBuilderImpl();
    }

    /**
     * Fluid builder interface for map.
     */
    public interface Builder {

        Builder putNull( String name );

        Builder put( String name, Byte v );

        Builder put( String name, Short v );

        Builder put( String name, Integer v );

        Builder put( String name, Long v );

        Builder put( String name, BigInteger v );

        Builder put( String name, BigDecimal v );

        Builder put( String name, Float v );

        Builder put( String name, Double v );

        Builder put( String name, String v );

        Builder put( String name, Boolean v );

        Builder put( String name, Value v );

        ValueMap build();

    }

}
