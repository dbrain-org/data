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
import org.dbrain.data.Value;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.stream.Collectors;

/**
 * A list of simple values.
 */
public class ListImpl implements Value.List {

    private final java.util.List<Value> delegate;

    private ListImpl( java.util.List<Value> delegate ) {
        this.delegate = delegate;
    }

    public ListImpl() {
        this( new ArrayList<>() );
    }

    @Override
    public Object getObject() {
        return stream().map( a -> a.getObject() ).collect( Collectors.toList() );
    }

    @Override
    public Object getObject( int fieldIndex ) {
        return get( fieldIndex ).getObject();
    }

    @Override
    public Boolean getBoolean() {
        throw new DataCoercionException( "Cannot cast list to boolean." );
    }

    @Override
    public String getString() {
        throw new DataCoercionException( "Cannot cast list to string." );
    }

    @Override
    public MapImpl getMap() {
        throw new DataCoercionException( "Cannot convert list to map." );
    }

    @Override
    public ListImpl getList() {
        return this;
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
    public boolean contains( Object o ) {
        return delegate.contains( o );
    }

    @Override
    public Iterator<Value> iterator() {
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
    public boolean add( Value jsonValue ) {
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
    public boolean addAll( Collection<? extends Value> c ) {
        return delegate.addAll( c );
    }

    @Override
    public boolean addAll( int index, Collection<? extends Value> c ) {
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
    public Value get( int index ) {
        return delegate.get( index );
    }

    @Override
    public Value set( int index, Value element ) {
        return delegate.set( index, element );
    }

    @Override
    public void add( int index, Value element ) {
        delegate.add( index, element );
    }

    @Override
    public Value remove( int index ) {
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
    public ListIterator<Value> listIterator() {
        return delegate.listIterator();
    }

    @Override
    public ListIterator<Value> listIterator( int index ) {
        return delegate.listIterator( index );
    }

    @Override
    public java.util.List<Value> subList( int fromIndex, int toIndex ) {
        return delegate.subList( fromIndex, toIndex );
    }

}
