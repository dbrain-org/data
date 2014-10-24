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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

/**
 * Created by epoitras on 26/06/14.
 */
public class JsonList extends JsonValue implements List<JsonValue> {

    private final List<JsonValue> delegate;

    JsonList( List<JsonValue> delegate ) {
        this.delegate = delegate;
    }

    JsonList() {
        this( new ArrayList<>() );
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
    public JsonMap asMap() {
        throw new UnsupportedOperationException();
    }

    @Override
    public org.dbrain.data.json.JsonList asList() {
        return this;
    }

    public String getString( int index ) {
        return get( index ).asString();
    }

    public <T> T getStringAs( int index, Function<String, T> f ) {
        return f.apply( getString( index ) );
    }

    public Double getDouble( int index ) {
        return get( index ).asDouble();
    }

    public <T> T getDoubleAs( int index, Function<Double, T> f ) {
        return f.apply( getDouble( index ) );
    }

    public Boolean getBoolean( int index ) {
        return get( index ).asBoolean();
    }

    public <T> T getBooleanAs( int index, Function<Boolean, T> f ) {
        return f.apply( getBoolean( index ) );
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
    public boolean contains( Object o ) {
        return delegate.contains( o );
    }

    @Override
    public Iterator<JsonValue> iterator() {
        return delegate.iterator();
    }

    @Override
    public Object[] toArray() {
        return delegate.toArray();
    }

    @Override
    public <T> T[] toArray( T[] a ) {
        return delegate.toArray( a );
    }

    @Override
    public boolean add( JsonValue jsonValue ) {
        return delegate.add( jsonValue );
    }

    @Override
    public boolean remove( Object o ) {
        return delegate.remove( o );
    }

    @Override
    public boolean containsAll( Collection<?> c ) {
        return delegate.containsAll( c );
    }

    @Override
    public boolean addAll( Collection<? extends JsonValue> c ) {
        return delegate.addAll( c );
    }

    @Override
    public boolean addAll( int index, Collection<? extends JsonValue> c ) {
        return delegate.addAll( index, c );
    }

    @Override
    public boolean removeAll( Collection<?> c ) {
        return delegate.removeAll( c );
    }

    @Override
    public boolean retainAll( Collection<?> c ) {
        return delegate.retainAll( c );
    }

    @Override
    public void replaceAll( UnaryOperator<JsonValue> operator ) {
        delegate.replaceAll( operator );
    }

    @Override
    public void sort( Comparator<? super JsonValue> c ) {
        delegate.sort( c );
    }

    @Override
    public void clear() {
        delegate.clear();
    }

    @Override
    public boolean equals( Object o ) {
        return delegate.equals( o );
    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }

    @Override
    public JsonValue get( int index ) {
        return delegate.get( index );
    }

    @Override
    public JsonValue set( int index, JsonValue element ) {
        return delegate.set( index, element );
    }

    @Override
    public void add( int index, JsonValue element ) {
        delegate.add( index, element );
    }

    @Override
    public JsonValue remove( int index ) {
        return delegate.remove( index );
    }

    @Override
    public int indexOf( Object o ) {
        return delegate.indexOf( o );
    }

    @Override
    public int lastIndexOf( Object o ) {
        return delegate.lastIndexOf( o );
    }

    @Override
    public ListIterator<JsonValue> listIterator() {
        return delegate.listIterator();
    }

    @Override
    public ListIterator<JsonValue> listIterator( int index ) {
        return delegate.listIterator( index );
    }

    @Override
    public List<JsonValue> subList( int fromIndex, int toIndex ) {
        return delegate.subList( fromIndex, toIndex );
    }

    @Override
    public Spliterator<JsonValue> spliterator() {
        return delegate.spliterator();
    }

    @Override
    public boolean removeIf( Predicate<? super JsonValue> filter ) {
        return delegate.removeIf( filter );
    }

    @Override
    public Stream<JsonValue> stream() {
        return delegate.stream();
    }

    @Override
    public Stream<JsonValue> parallelStream() {
        return delegate.parallelStream();
    }

    @Override
    public void forEach( Consumer<? super JsonValue> action ) {
        delegate.forEach( action );
    }
}
