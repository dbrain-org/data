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
import org.dbrain.data.json.JsonParser;

import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;

/**
 * A simple value can only contains primitive values.
 */
public interface Value extends FieldAccessors {

    static Value.List newList() {
        return new ListImpl();
    }

    static Value of( String s ) {
        return s != null ? new ValueImpl( s ) : null;
    }

    static Value of( Double d ) {
        return d != null ? new ValueImpl( d ) : null;
    }

    static Value of( Boolean b ) {
        if ( b != null ) {
            return b ? ValueImpl.TRUE : ValueImpl.FALSE;
        } else {
            return null;
        }
    }

    static Value ofJson( String jsonString ) {
        return ofJson( new JsonParser( new StringReader( jsonString ) ), true );
    }

    static Value ofJson( Reader json ) {
        return ofJson( new JsonParser( json ) );
    }

    static Value ofJson( JsonParser parser ) {
        return ofJson( parser, true );
    }

    static Value ofJson( JsonParser parser, boolean validateEof ) {
        if ( parser.getToken() != null ) {
            Value result;
            switch ( parser.getToken() ) {
                case STRING:
                    result = of( parser.readString() );
                    break;
                case DOUBLE:
                    result = of( parser.readDouble() );
                    break;
                case BOOLEAN:
                    result = of( parser.readBoolean() );
                    break;
                case NULL:
                    parser.readToken( JsonParser.Token.NULL );
                    result = null;
                    break;
                case OPEN_OBJECT: {
                    parser.readToken( JsonParser.Token.OPEN_OBJECT );
                    HashMap<String, Value> values = new HashMap<>();
                    while ( parser.getToken() != JsonParser.Token.CLOSE_OBJECT ) {
                        // Skip commas.
                        while ( parser.getToken() == JsonParser.Token.COMMA ) {
                            parser.readToken( JsonParser.Token.COMMA );
                        }
                        if ( parser.getToken() != JsonParser.Token.CLOSE_OBJECT ) {
                            String key = parser.readString();
                            parser.readToken( JsonParser.Token.COLON );
                            Value value = ofJson( parser, false );
                            values.put( key, value );
                        }
                    }
                    parser.readToken( JsonParser.Token.CLOSE_OBJECT );
                    result = new MapImpl( values );
                }
                break;
                case OPEN_ARRAY: {
                    parser.readToken( JsonParser.Token.OPEN_ARRAY );
                    Value.List values = Value.newList();
                    while ( parser.getToken() != JsonParser.Token.CLOSE_ARRAY ) {
                        // Skip commas.
                        while ( parser.getToken() == JsonParser.Token.COMMA ) {
                            parser.readToken( JsonParser.Token.COMMA );
                        }
                        if ( parser.getToken() != JsonParser.Token.CLOSE_ARRAY ) {
                            values.add( ofJson( parser, false ) );
                        }
                    }
                    parser.readToken( JsonParser.Token.CLOSE_ARRAY );
                    result = values;
                }
                break;
                default:
                    throw parser.error( "Expected value" );
            }
            if ( validateEof ) {
                if ( parser.getToken() != null ) {
                    throw parser.error( "Expected end of file." );
                }
            }

            return result;
        } else {
            return null;
        }

    }

    Map asMap();

    Value.List asList();

    public interface Map extends Value, java.util.Map<String, Value>, NamedFieldsAccessors {

    }

    public interface List extends Value, java.util.List<Value>, IndexedFieldsAccessors {

    }
}
