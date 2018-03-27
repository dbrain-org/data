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

package org.dbrain.data.tree;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tabular.NamedFieldAccessors;
import org.dbrain.data.impl.value.MapValueImpl;
import org.dbrain.data.impl.value.ValueMapBuilderImpl;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.function.Function;

/**
 * An map of value.
 */
public interface NodeMap extends Node, java.util.Map<String, Node>, NamedFieldAccessors {

    /**
     * Create a new empty map.
     */
    static NodeMap newInstance() {
        return new MapValueImpl();
    }

    /**
     * From a map of strings.
     */
    static NodeMap of(Map<String, ?> v ) {
        NodeMap result = newInstance();
        for ( Map.Entry<String, ?> e : v.entrySet() ) {
            result.put( e.getKey(), Node.of( e.getValue() ) );
        }
        return result;
    }

    /**
     * Convert a Map to a value map using a key mapper.
     */
    static NodeMap of(Map<?, ?> v, Function<Object, String> keyMapper ) {
        NodeMap result = newInstance();
        for ( Map.Entry<?, ?> e : v.entrySet() ) {
            String key = keyMapper.apply( e.getKey() );
            Node oldNode = result.put( key, Node.of( e.getValue() ) );
            if ( oldNode != null ) {
                throw new DataCoercionException( "Duplicate value when casting to map: " + key );
            }
        }
        return result;
    }

    /**
     * Create a new builder.
     */
    static NodeMap.Builder newBuilder() {
        return new ValueMapBuilderImpl();
    }

    /**
     * Fluid builder interface for map.
     */
    interface Builder {

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

        Builder put( String name, Node v );

        NodeMap build();

    }

}
