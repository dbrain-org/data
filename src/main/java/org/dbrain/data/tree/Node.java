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

package org.dbrain.data.tree;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tabular.FieldAccessors;
import org.dbrain.data.impl.value.BoolValueImpl;
import org.dbrain.data.impl.value.NullValueImpl;
import org.dbrain.data.impl.value.NumberValueImpl;
import org.dbrain.data.impl.value.StringValueImpl;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.function.Function;

/**
 * A simple value that can only contains primitive values.
 */
public interface Node extends FieldAccessors {

    static Node nullValue() {
        return NullValueImpl.NULL;
    }

    static Node of(String s ) {
        return s != null ? new StringValueImpl( s ) : nullValue();
    }

    static Node of(CharSequence s ) {
        return s != null ? new StringValueImpl( s.toString() ) : nullValue();
    }

    static Node of(Byte b ) {
        return b != null ? new NumberValueImpl( b ) : nullValue();
    }

    static Node of(Short s ) {
        return s != null ? new NumberValueImpl( s ) : nullValue();
    }

    static Node of(Integer i ) {
        return i != null ? new NumberValueImpl( i ) : nullValue();
    }

    static Node of(Long l ) {
        return l != null ? new NumberValueImpl( l ) : nullValue();
    }

    static Node of(BigInteger bi ) {
        return bi != null ? new NumberValueImpl( bi ) : nullValue();
    }

    static Node of(BigDecimal bd ) {
        return bd != null ? new NumberValueImpl( bd ) : nullValue();
    }

    static Node of(Float v ) {
        if ( v != null ) {
            float value = v.floatValue();
            if ( Float.isNaN( value ) ) {
                return nullValue();
            } else if ( Float.isFinite( value ) ) {
                return new NumberValueImpl( value );
            } else {
                throw new DataCoercionException( "Value cannot contain infinity." );
            }
        } else {
            return nullValue();
        }
    }

    static Node of(Double v ) {
        if ( v != null ) {
            double doubleValue = v.doubleValue();
            if ( Double.isNaN( doubleValue ) ) {
                return nullValue();
            } else if ( Double.isFinite( doubleValue ) ) {
                return new NumberValueImpl( doubleValue );
            } else {
                throw new DataCoercionException( "Value cannot contain infinity." );
            }
        } else {
            return nullValue();
        }
    }

    static Node of(Boolean b ) {
        if ( b != null ) {
            return b ? BoolValueImpl.TRUE : BoolValueImpl.FALSE;
        } else {
            return nullValue();
        }
    }

    /**
     * Make sure Value is not null.
     */
    static Node of(Node v ) {
        return v != null ? v : nullValue();
    }

    /**
     * Cast of one of the primitive type.
     */
    static Node of(Object v, Function<Object, Node> valueFromObject ) {
        if ( v == null ) {
            return nullValue();
        } else if ( v instanceof Node) {
            return (Node) v;
        } else if ( v instanceof String ) {
            return Node.of( (String) v );
        } else if ( v instanceof Byte ) {
            return Node.of( (Byte) v );
        } else if ( v instanceof Short ) {
            return Node.of( (Short) v );
        } else if ( v instanceof Integer ) {
            return Node.of( (Integer) v );
        } else if ( v instanceof Long ) {
            return Node.of( (Long) v );
        } else if ( v instanceof BigDecimal ) {
            return Node.of( (BigDecimal) v );
        } else if ( v instanceof BigInteger ) {
            return Node.of( (BigInteger) v );
        } else if ( v instanceof Float ) {
            return Node.of( (Float) v );
        } else if ( v instanceof Double ) {
            return Node.of( (Double) v );
        } else if ( v instanceof Boolean ) {
            return Node.of( (Boolean) v );
        } else if ( v instanceof Map ) {
            return NodeMap.of( (Map) v );
        } else if ( v instanceof Iterable ) {
            return NodeList.of( (Iterable) v );
        } else {
            return valueFromObject.apply( v );
        }
    }

    static Node of(Object o ) {
        return of( o, o1 -> {
            throw new DataCoercionException( "Cannot cast " + o1.getClass().getName() + " to value." );
        } );
    }

    static Object toObject( Node node) {
        if ( node == null ) {
            return null;
        } else {
            return node.getObject();
        }
    }

    NodeMap getMap();

    NodeList getList();

    boolean isNull();

}
