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

package org.dbrain.data;

import org.dbrain.data.access.FieldAccessors;
import org.dbrain.data.impl.value.BoolValueImpl;
import org.dbrain.data.impl.value.NullValueImpl;
import org.dbrain.data.impl.value.NumberValueImpl;
import org.dbrain.data.impl.value.StringValueImpl;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

/**
 * A simple value that can only contains primitive values.
 */
public interface Value extends FieldAccessors {

    static Value of( String s ) {
        return s != null ? new StringValueImpl( s ) : NullValueImpl.NULL;
    }

    static Value of( CharSequence s ) {
        return s != null ? new StringValueImpl( s.toString() ) : NullValueImpl.NULL;
    }

    static Value of( Byte b ) {
        return b != null ? new NumberValueImpl( b ) : NullValueImpl.NULL;
    }

    static Value of( Short s ) {
        return s != null ? new NumberValueImpl( s ) : NullValueImpl.NULL;
    }

    static Value of( Integer i ) {
        return i != null ? new NumberValueImpl( i ) : NullValueImpl.NULL;
    }

    static Value of( Long l ) {
        return l != null ? new NumberValueImpl( l ) : NullValueImpl.NULL;
    }

    static Value of( BigInteger bi ) {
        return bi != null ? new NumberValueImpl( bi ) : NullValueImpl.NULL;
    }

    static Value of( BigDecimal bd ) {
        return bd != null ? new NumberValueImpl( bd ) : NullValueImpl.NULL;
    }

    static Value of( Float f ) {
        return f != null ? new NumberValueImpl( f ) : NullValueImpl.NULL;
    }

    static Value of( Double d ) {
        return d != null ? new NumberValueImpl( d ) : NullValueImpl.NULL;
    }

    static Value of( Boolean b ) {
        if ( b != null ) {
            return b ? BoolValueImpl.TRUE : BoolValueImpl.FALSE;
        } else {
            return NullValueImpl.NULL;
        }
    }

    /**
     * Make sure Value is not null.
     */
    static Value of( Value v ) {
        return v != null ? v : NullValueImpl.NULL;
    }

    /**
     * Cast of one of the primitive type.
     */
    static Value of( Object v, Function<Object, Value> valueFromObject ) {
        if ( v == null ) {
            return NullValueImpl.NULL;
        } else if ( v instanceof Value ) {
            return (Value) v;
        } else if ( v instanceof String ) {
            return Value.of( (String) v );
        } else if ( v instanceof Byte ) {
            return Value.of( (Byte) v );
        } else if ( v instanceof Short ) {
            return Value.of( (Short) v );
        } else if ( v instanceof Integer ) {
            return Value.of( (Integer) v );
        } else if ( v instanceof Long ) {
            return Value.of( (Long) v );
        } else if ( v instanceof BigDecimal ) {
            return Value.of( (BigDecimal) v );
        } else if ( v instanceof BigInteger ) {
            return Value.of( (BigInteger) v );
        } else if ( v instanceof Float ) {
            return Value.of( (Float) v );
        } else if ( v instanceof Double ) {
            return Value.of( (Double) v );
        } else if ( v instanceof Boolean ) {
            return Value.of( (Boolean) v );
        } else {
            return valueFromObject.apply( v );
        }
    }

    static Value of( Object o ) {
        return of( o, o1 -> {
            throw new DataCoercionException( "Cannot cast " + o1.getClass().getName() + " to value." );
        } );
    }

    ValueMap getMap();

    ValueList getList();

    boolean isNull();

}
