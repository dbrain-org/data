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

import org.dbrain.data.Value;

import java.util.Collection;
import java.util.HashMap;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Created by epoitras on 26/06/14.
 */
public class MapImpl implements Value.Map {

    private final HashMap<String, Value> delegate;

    public MapImpl( HashMap<String, Value> delegate ) {
        this.delegate = delegate;
    }

    public MapImpl() {
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
    public MapImpl asMap() {
        return this;
    }

    @Override
    public Value.List asList() {
        throw new UnsupportedOperationException();
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
    public Value get( Object key ) {
        return delegate.get( key );
    }

    @Override
    public boolean containsKey( Object key ) {
        return delegate.containsKey( key );
    }

    @Override
    public Value put( String key, Value value ) {
        return delegate.put( key, value );
    }

    @Override
    public void putAll( java.util.Map<? extends String, ? extends Value> m ) {
        delegate.putAll( m );
    }

    @Override
    public Value remove( Object key ) {
        return delegate.remove( key );
    }

    @Override
    public void clear() {
        delegate.clear();
    }

    @Override
    public boolean containsValue( Object value ) {
        return delegate.containsValue( value );
    }

    @Override
    public Set<String> keySet() {
        return delegate.keySet();
    }

    @Override
    public Collection<Value> values() {
        return delegate.values();
    }

    @Override
    public Set<Entry<String, Value>> entrySet() {
        return delegate.entrySet();
    }

    @Override
    public Value getOrDefault( Object key, Value defaultValue ) {
        return delegate.getOrDefault( key, defaultValue );
    }

    @Override
    public Value putIfAbsent( String key, Value value ) {
        return delegate.putIfAbsent( key, value );
    }

    @Override
    public boolean remove( Object key, Object value ) {
        return delegate.remove( key, value );
    }

    @Override
    public boolean replace( String key, Value oldValue, Value newValue ) {
        return delegate.replace( key, oldValue, newValue );
    }

    @Override
    public Value replace( String key, Value value ) {
        return delegate.replace( key, value );
    }

    @Override
    public Value computeIfAbsent( String key, Function<? super String, ? extends Value> mappingFunction ) {
        return delegate.computeIfAbsent( key, mappingFunction );
    }

    @Override
    public Value computeIfPresent( String key,
                                   BiFunction<? super String, ? super Value, ? extends Value> remappingFunction ) {
        return delegate.computeIfPresent( key, remappingFunction );
    }

    @Override
    public Value compute( String key, BiFunction<? super String, ? super Value, ? extends Value> remappingFunction ) {
        return delegate.compute( key, remappingFunction );
    }

    @Override
    public Value merge( String key,
                        Value value,
                        BiFunction<? super Value, ? super Value, ? extends Value> remappingFunction ) {
        return delegate.merge( key, value, remappingFunction );
    }

    @Override
    public void forEach( BiConsumer<? super String, ? super Value> action ) {
        delegate.forEach( action );
    }

    @Override
    public void replaceAll( BiFunction<? super String, ? super Value, ? extends Value> function ) {
        delegate.replaceAll( function );
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        MapImpl jsonMap = (MapImpl) o;

        return delegate.equals( jsonMap.delegate );

    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }

}
