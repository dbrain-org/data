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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.stream.Collectors;

/**
 * A list of simple values.
 */
public class ListValueImpl implements NodeList {

    private final java.util.List<Node> delegate;

    private ListValueImpl( java.util.List<Node> delegate ) {
        this.delegate = delegate;
    }

    public ListValueImpl() {
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
    public MapValueImpl getMap() {
        throw new DataCoercionException( "Cannot convert list to map." );
    }

    @Override
    public ListValueImpl getList() {
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
    public Iterator<Node> iterator() {
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
    public boolean add( Node jsonNode) {
        return delegate.add(jsonNode);
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
    public boolean addAll( Collection<? extends Node> c ) {
        return delegate.addAll( c );
    }

    @Override
    public boolean addAll( int index, Collection<? extends Node> c ) {
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
    public String toString() {
        return delegate.toString();
    }

    @Override
    public Node get(int index ) {
        return delegate.get( index );
    }

    @Override
    public Node set(int index, Node element ) {
        return delegate.set( index, element );
    }

    @Override
    public void add( int index, Node element ) {
        delegate.add( index, element );
    }

    @Override
    public Node remove(int index ) {
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
    public ListIterator<Node> listIterator() {
        return delegate.listIterator();
    }

    @Override
    public ListIterator<Node> listIterator(int index ) {
        return delegate.listIterator( index );
    }

    @Override
    public java.util.List<Node> subList(int fromIndex, int toIndex ) {
        return delegate.subList( fromIndex, toIndex );
    }

}
