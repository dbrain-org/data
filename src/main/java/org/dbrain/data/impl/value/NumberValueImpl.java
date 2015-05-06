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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

/**
 * Wrap a number value;
 */
public final class NumberValueImpl implements Value {

    private final BigDecimal value;

    public NumberValueImpl( Byte value ) {
        Objects.requireNonNull( value );
        this.value = new BigDecimal( value );
    }

    public NumberValueImpl( Short value ) {
        Objects.requireNonNull( value );
        this.value = new BigDecimal( value );
    }

    public NumberValueImpl( Integer value ) {
        Objects.requireNonNull( value );
        this.value = new BigDecimal( value );
    }

    public NumberValueImpl( Long value ) {
        Objects.requireNonNull( value );
        this.value = new BigDecimal( value );
    }

    public NumberValueImpl( BigInteger value ) {
        Objects.requireNonNull( value );
        this.value = new BigDecimal( value );
    }

    public NumberValueImpl( BigDecimal value ) {
        Objects.requireNonNull( value );
        this.value = value;
    }

    public NumberValueImpl( float value ) {
        if ( !Float.isFinite( value ) ) throw new IllegalArgumentException();
        this.value = new BigDecimal( Float.toString( value ) ).stripTrailingZeros();
    }

    public NumberValueImpl( double value ) {
        if ( !Double.isFinite( value ) ) throw new IllegalArgumentException();
        this.value = new BigDecimal( Double.toString( value ) ).stripTrailingZeros();
    }

    public Object getObject() {
        return value;
    }

    @Override
    public MapValueImpl getMap() {
        throw new DataCoercionException( "Cannot cast number to Map." );
    }

    @Override
    public ListValueImpl getList() {
        throw new DataCoercionException( "Cannot cast number to List." );
    }

    @Override
    public boolean isNull() {
        return value == null;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        NumberValueImpl that = (NumberValueImpl) o;
        if ( value.compareTo( that.value ) != 0 ) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
