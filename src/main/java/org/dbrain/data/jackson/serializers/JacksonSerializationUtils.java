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

package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import org.dbrain.data.Value;
import org.dbrain.data.ValueList;
import org.dbrain.data.ValueMap;
import org.dbrain.data.impl.value.NullValueImpl;

import java.io.IOException;

/**
 * Created by epoitras on 08/01/15.
 */
public class JacksonSerializationUtils {

    /**
     * Helper method to ensure to get the current token.
     */
    public static JsonToken getToken( JsonParser parser ) throws IOException {
        return parser.hasCurrentToken() ? parser.getCurrentToken() : parser.nextToken();
    }


    /**
     * Helper method to ensure to get the current token.
     */
    public static JsonToken currentToken( JsonParser parser ) throws IOException {
        return parser.hasCurrentToken() ? parser.getCurrentToken() : parser.nextValue();
    }

    public static Value parseValue( JsonParser parser ) throws IOException {
        JsonToken token = getToken( parser );
        if ( token != null ) {
            Value result;
            switch ( token ) {
                case VALUE_STRING:
                    result = Value.of( parser.getValueAsString() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NUMBER_FLOAT:
                    result = Value.of( parser.getDoubleValue() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NUMBER_INT:
                    result = Value.of( parser.getBigIntegerValue() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NULL:
                    result = NullValueImpl.NULL;
                    parser.clearCurrentToken();
                    break;
                case VALUE_TRUE:
                    result = Value.of( Boolean.TRUE );
                    parser.clearCurrentToken();
                    break;
                case VALUE_FALSE:
                    result = Value.of( Boolean.FALSE );
                    parser.clearCurrentToken();
                    break;
                case START_OBJECT: {
                    parser.clearCurrentToken();
                    ValueMap values = ValueMap.newInstance();
                    while ( currentToken( parser ) != JsonToken.END_OBJECT ) {
                        String key = parser.getCurrentName();
                        Value v = parseValue( parser );
                        if ( v == null ) {
                            throw new JsonParseException( "Expected JSON value.", parser.getCurrentLocation() );
                        }
                        values.put( key, v );
                    }
                    if ( currentToken( parser ) == JsonToken.END_OBJECT ) {
                        parser.clearCurrentToken();
                    } else {
                        throw new JsonParseException( "Expected end of object.", parser.getCurrentLocation() );
                    }
                    result = values;
                }
                break;
                case START_ARRAY: {
                    parser.clearCurrentToken();
                    ValueList values = ValueList.newInstance();
                    while ( currentToken( parser ) != JsonToken.END_ARRAY ) {
                        Value v = parseValue( parser );
                        if ( v == null ) {
                            throw new JsonParseException( "Expected value.", parser.getCurrentLocation() );
                        }
                        values.add( v );
                    }
                    if ( currentToken( parser ) == JsonToken.END_ARRAY ) {
                        parser.clearCurrentToken();
                    } else {
                        throw new JsonParseException( "Expected end of array.", parser.getCurrentLocation() );
                    }
                    result = values;
                }
                break;
                default:
                    throw new JsonParseException( "Expected value.", parser.getCurrentLocation() );
            }
            return result;
        } else {
            return null;
        }
    }
}
