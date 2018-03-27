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

package org.dbrain.data.impl.value;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;

import java.util.Collection;
import java.util.HashMap;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Implementation of the Value.Map.
 */
public class MapValueImpl implements NodeMap {

    private final HashMap<String, Node> delegate;

    public MapValueImpl( HashMap<String, Node> delegate ) {
        this.delegate = delegate;
    }

    public MapValueImpl() {
        this( new HashMap<>() );
    }

    public Object getObject() {
        java.util.Map<String, Object> result = new HashMap<>( size() );
        delegate.forEach( ( s, value ) -> result.put( s, value.getObject() ) );
        return result;
    }

    @Override
    public Object getObject( String fieldName ) {
        return get( fieldName ).getObject();
    }

    @Override
    public Boolean getBoolean() {
        throw new DataCoercionException( "Cannot cast map to boolean." );
    }

    @Override
    public Byte getByte() {
        throw new DataCoercionException( "Cannot cast map to byte." );
    }

    @Override
    public Short getShort() {
        throw new DataCoercionException( "Cannot cast map to short." );
    }

    @Override
    public Integer getInt() {
        throw new DataCoercionException( "Cannot cast map to integer." );
    }

    @Override
    public Long getLong() {
        throw new DataCoercionException( "Cannot cast map to long." );
    }

    @Override
    public Float getFloat() {
        throw new DataCoercionException( "Cannot cast map to float." );
    }

    @Override
    public Double getDouble() {
        throw new DataCoercionException( "Cannot cast map to double." );
    }

    @Override
    public String getString() {
        throw new DataCoercionException( "Cannot cast map to string." );
    }

    @Override
    public MapValueImpl getMap() {
        return this;
    }

    @Override
    public NodeList getList() {
        throw new DataCoercionException( "Cannot cast map to list." );
    }

    @Override
    public boolean isNull() {
        return false;
    }

    @Override
    public int size() {
        return delegate.size();
    }

    @Override
    public boolean isEmpty() {
        return delegate.isEmpty();
    }

    @Override
    public Node get(Object key ) {
        return Node.of( delegate.get( key ) );
    }

    @Override
    public boolean containsKey( Object key ) {
        return delegate.containsKey( key );
    }

    @Override
    public Node put(String key, Node node) {
        Objects.requireNonNull( key );
        return delegate.put( key, Node.of(node) );
    }

    @Override
    public void putAll( java.util.Map<? extends String, ? extends Node> m ) {
        for ( NodeMap.Entry<? extends String, ? extends Node> e : m.entrySet() ) {
            put( e.getKey(), e.getValue() );
        }
    }

    @Override
    public Node remove(Object key ) {
        return delegate.remove( key );
    }

    @Override
    public void clear() {
        delegate.clear();
    }

    @Override
    public boolean containsValue( Object value ) {
        return delegate.containsValue( Node.of( value ) );
    }

    @Override
    public Set<String> keySet() {
        return delegate.keySet();
    }

    @Override
    public Collection<Node> values() {
        return delegate.values();
    }

    @Override
    public Set<Entry<String, Node>> entrySet() {
        return delegate.entrySet();
    }

    @Override
    public Node getOrDefault(Object key, Node defaultNode) {
        Node result = get( key );
        return result.isNull() ? Node.of(defaultNode) : result;
    }

    @Override
    public Node putIfAbsent(String key, Node node) {
        if ( !containsKey( key ) ) {
            return put( key, Node.of(node) );
        } else {
            return get( key );
        }
    }

    @Override
    public boolean remove( Object key, Object value ) {
        return delegate.remove( key, Node.of( value ) );
    }

    @Override
    public boolean replace(String key, Node oldNode, Node newNode) {
        return delegate.replace( key, Node.of(oldNode), Node.of(newNode) );
    }

    @Override
    public Node replace(String key, Node node) {
        return delegate.replace( key, Node.of(node) );
    }

    @Override
    public Node computeIfAbsent(String key, Function<? super String, ? extends Node> mappingFunction ) {
        return delegate.computeIfAbsent( key, mappingFunction );
    }

    @Override
    public Node computeIfPresent(String key,
                                 BiFunction<? super String, ? super Node, ? extends Node> remappingFunction ) {
        return delegate.computeIfPresent( key, remappingFunction );
    }

    @Override
    public Node compute(String key, BiFunction<? super String, ? super Node, ? extends Node> remappingFunction ) {
        return delegate.compute( key, remappingFunction );
    }

    @Override
    public Node merge(String key,
                      Node node,
                      BiFunction<? super Node, ? super Node, ? extends Node> remappingFunction ) {
        return delegate.merge( key, node, remappingFunction );
    }

    @Override
    public void forEach( BiConsumer<? super String, ? super Node> action ) {
        delegate.forEach( action );
    }

    @Override
    public void replaceAll( BiFunction<? super String, ? super Node, ? extends Node> function ) {
        delegate.replaceAll( function );
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        return delegate.equals( o );

    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }

    @Override
    public String toString() {
        return delegate.toString();
    }

}
