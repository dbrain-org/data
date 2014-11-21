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

package org.dbrain.data.json;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Created by epoitras on 26/06/14.
 */
public class JsonMap extends JsonValue implements Map<String, JsonValue> {

    private final HashMap<String, JsonValue> delegate;

    JsonMap( HashMap<String, JsonValue> delegate ) {
        this.delegate = delegate;
    }

    public JsonMap() {
        this( new HashMap<>() );
    }

    @Override
    public String asString() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Double asDouble() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Boolean asBoolean() {
        throw new UnsupportedOperationException();
    }

    @Override
    public org.dbrain.data.json.JsonMap asMap() {
        return this;
    }

    @Override
    public JsonList asList() {
        throw new UnsupportedOperationException();
    }

    public String getString( String name ) {
        JsonValue value = get( name );
        return value != null ? value.asString() : null;
    }

    public <T> T getStringAs( String name, Function<? super String, T> f ) {
        return f.apply( getString( name ) );
    }

    public Double getDouble( String name ) {
        JsonValue value = get( name );
        return value != null ? value.asDouble() : null;
    }

    public <T> T getDoubleAs( String name, Function<? super Double, T> f ) {
        return f.apply( getDouble( name ) );
    }

    public Boolean getBoolean( String name ) {
        JsonValue value = get( name );
        return value != null ? value.asBoolean() : null;
    }

    public <T> T getBoolean( String name, Function<? super Boolean, T> f ) {
        return f.apply( getBoolean( name ) );
    }

    public org.dbrain.data.json.JsonMap getMap( String name ) {
        JsonValue value = get( name );
        return value != null ? value.asMap() : null;
    }

    public JsonList getList( String name ) {
        JsonValue value = get( name );
        return value != null ? value.asList() : null;
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
    public JsonValue get( Object key ) {
        return delegate.get( key );
    }

    @Override
    public boolean containsKey( Object key ) {
        return delegate.containsKey( key );
    }

    @Override
    public JsonValue put( String key, JsonValue value ) {
        return delegate.put( key, value );
    }

    @Override
    public void putAll( Map<? extends String, ? extends JsonValue> m ) {
        delegate.putAll( m );
    }

    @Override
    public JsonValue remove( Object key ) {
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
    public Collection<JsonValue> values() {
        return delegate.values();
    }

    @Override
    public Set<Entry<String, JsonValue>> entrySet() {
        return delegate.entrySet();
    }

    @Override
    public JsonValue getOrDefault( Object key, JsonValue defaultValue ) {
        return delegate.getOrDefault( key, defaultValue );
    }

    @Override
    public JsonValue putIfAbsent( String key, JsonValue value ) {
        return delegate.putIfAbsent( key, value );
    }

    @Override
    public boolean remove( Object key, Object value ) {
        return delegate.remove( key, value );
    }

    @Override
    public boolean replace( String key, JsonValue oldValue, JsonValue newValue ) {
        return delegate.replace( key, oldValue, newValue );
    }

    @Override
    public JsonValue replace( String key, JsonValue value ) {
        return delegate.replace( key, value );
    }

    @Override
    public JsonValue computeIfAbsent( String key, Function<? super String, ? extends JsonValue> mappingFunction ) {
        return delegate.computeIfAbsent( key, mappingFunction );
    }

    @Override
    public JsonValue computeIfPresent( String key,
                                       BiFunction<? super String, ? super JsonValue, ? extends JsonValue> remappingFunction ) {
        return delegate.computeIfPresent( key, remappingFunction );
    }

    @Override
    public JsonValue compute( String key,
                              BiFunction<? super String, ? super JsonValue, ? extends JsonValue> remappingFunction ) {
        return delegate.compute( key, remappingFunction );
    }

    @Override
    public JsonValue merge( String key,
                            JsonValue value,
                            BiFunction<? super JsonValue, ? super JsonValue, ? extends JsonValue> remappingFunction ) {
        return delegate.merge( key, value, remappingFunction );
    }

    @Override
    public void forEach( BiConsumer<? super String, ? super JsonValue> action ) {
        delegate.forEach( action );
    }

    @Override
    public void replaceAll( BiFunction<? super String, ? super JsonValue, ? extends JsonValue> function ) {
        delegate.replaceAll( function );
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        org.dbrain.data.json.JsonMap jsonMap = (org.dbrain.data.json.JsonMap) o;

        return delegate.equals( jsonMap.delegate );

    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }

}
