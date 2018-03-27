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

import org.dbrain.data.tabular.IndexedFieldAccessors;
import org.dbrain.data.impl.value.ListValueImpl;
import org.dbrain.data.impl.value.ValueListBuilderImpl;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * An ordered list of value.
 */
public interface NodeList extends Node, java.util.List<Node>, IndexedFieldAccessors {

    /**
     * Create a new empty list.
     */
    static NodeList newInstance() {
        return new ListValueImpl();
    }

    /**
     * Create a new builder.
     */
    static NodeList.Builder newBuilder() {
        return new ValueListBuilderImpl();
    }

    /**
     * List from array.
     */
    static <T extends Object> NodeList asList(Object... list ) {
        return of( list );
    }

    /**
     * List from array.
     */
    static <T extends Object> NodeList of(T[] list ) {
        if ( list != null ) {
            NodeList result = newInstance();
            for ( Object o : list ) {
                result.add( Node.of( o ) );
            }
            return result;
        } else {
            return null;
        }
    }

    /**
     * Create a ValueList from a generic list.
     */
    static NodeList of(Iterable<Object> list ) {
        if ( list != null ) {
            NodeList result = newInstance();
            for ( Object o : list ) {
                result.add( Node.of( o ) );
            }
            return result;
        } else {
            return null;
        }
    }

    /**
     * Fluid builder interface for list.
     */
    interface Builder {

        Builder addNull();

        Builder add( Byte v );

        Builder add( Short v );

        Builder add( Integer v );

        Builder add( Long v );

        Builder add( BigInteger v );

        Builder add( BigDecimal v );

        Builder add( Float v );

        Builder add( Double v );

        Builder add( String v );

        Builder add( Boolean v );

        Builder add( Node v );

        NodeList build();

    }

}
