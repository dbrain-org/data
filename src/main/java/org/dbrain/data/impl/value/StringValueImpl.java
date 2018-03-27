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

package org.dbrain.data.impl.value;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tree.Node;

import java.util.Objects;

/**
 * Wrap a scalar value;
 */
public final class StringValueImpl implements Node {

    private final String value;

    public StringValueImpl( String value ) {
        Objects.requireNonNull( value );
        this.value = value;
    }

    public Object getObject() {
        return value;
    }

    @Override
    public MapValueImpl getMap() {
        throw new DataCoercionException( "Cannot cast string to Map." );
    }

    @Override
    public ListValueImpl getList() {
        throw new DataCoercionException( "Cannot cast string to List." );
    }

    @Override
    public boolean isNull() {
        return value == null;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        StringValueImpl value1 = (StringValueImpl) o;

        return !( value != null ? !value.equals( value1.value ) : value1.value != null );

    }

    @Override
    public int hashCode() {
        return value != null ? value.hashCode() : 0;
    }

    @Override
    public String toString() {
        return value;
    }
}
