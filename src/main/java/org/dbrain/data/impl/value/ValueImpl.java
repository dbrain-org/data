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
import org.dbrain.data.impl.json.JsonBridge;

/**
 * Wrap a scalar value;
 */
public final class ValueImpl implements Value {

    public static final Value TRUE  = new ValueImpl( Boolean.TRUE );
    public static final Value FALSE = new ValueImpl( Boolean.FALSE );
    public static final Value NULL  = new ValueImpl( null );

    private final Object value;

    public ValueImpl( Object value ) {
        this.value = value;
    }

    public Object getObject() {
        return value;
    }

    @Override
    public MapImpl getMap() {
        throw new DataCoercionException( "Cannot cast value to Map.");
    }

    @Override
    public ListImpl getList() {
        throw new DataCoercionException( "Cannot cast value to List.");
    }

    @Override
    public boolean isNull() {
        return value == null;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        ValueImpl value1 = (ValueImpl) o;

        return !( value != null ? !value.equals( value1.value ) : value1.value != null );

    }

    @Override
    public int hashCode() {
        return value != null ? value.hashCode() : 0;
    }

    @Override
    public String toString() {
        return JsonBridge.get().writeToString( this );
    }
}
