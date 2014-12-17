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
import org.dbrain.data.access.IndexedFieldsAccessors;
import org.dbrain.data.access.NamedFieldsAccessors;
import org.dbrain.data.impl.value.ListImpl;
import org.dbrain.data.impl.value.MapImpl;
import org.dbrain.data.impl.value.ValueImpl;
import org.dbrain.data.impl.value.ValueJsonBridge;

import java.io.Reader;

/**
 * A simple value can only contains primitive values.
 */
public interface Value extends FieldAccessors {

    /**
     * Create a new empty list.
     */
    static Value.List newList() {
        return new ListImpl();
    }

    /**
     * Create a new empty map.
     */
    static Value.Map newMap() {
        return new MapImpl();
    }

    static Value of( String s ) {
        return s != null ? new ValueImpl( s ) : ValueImpl.NULL;
    }

    static Value of( Byte b ) {
        return b != null ? new ValueImpl( b ) : ValueImpl.NULL;
    }

    static Value of( Short s ) {
        return s != null ? new ValueImpl( s ) : ValueImpl.NULL;
    }
    static Value of( Integer i ) {
        return i != null ? new ValueImpl( i ) : ValueImpl.NULL;
    }

    static Value of( Long l ) {
        return l != null ? new ValueImpl( l ) : ValueImpl.NULL;
    }

    static Value of( Float f ) {
        return f != null ? new ValueImpl( f ) : ValueImpl.NULL;
    }

    static Value of( Double d ) {
        return d != null ? new ValueImpl( d ) : ValueImpl.NULL;
    }

    static Value of( Boolean b ) {
        if ( b != null ) {
            return b ? ValueImpl.TRUE : ValueImpl.FALSE;
        } else {
            return ValueImpl.NULL;
        }
    }

    /**
     * Make sure Value is not null.
     */
    static Value of( Value v ) {
        return v != null ? v : ValueImpl.NULL;
    }

    static Value of( Object v ) {
        if ( v == null ) {
            return ValueImpl.NULL;
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
        } else if ( v instanceof Float ) {
            return Value.of( (Float) v );
        } else if ( v instanceof Double ) {
            return Value.of( (Double) v );
        } else if ( v instanceof Boolean ) {
            return Value.of( (Boolean) v );
        } else {
            // Can be replaced here with dynamic object mapping.
            throw new IllegalStateException();
        }
    }


    static Value ofJson( String jsonString ) {
        return ValueJsonBridge.ofJson( jsonString );
    }

    static Value ofJson( Reader json ) {
        return ValueJsonBridge.ofJson( json );
    }

    Map getMap();

    Value.List getList();

    boolean isNull();

    public interface Map extends Value, java.util.Map<String, Value>, NamedFieldsAccessors {}

    public interface List extends Value, java.util.List<Value>, IndexedFieldsAccessors {}
}
