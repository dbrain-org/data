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

package org.dbrain.data.impl.value.json;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import org.dbrain.data.Value;
import org.dbrain.data.impl.value.ListImpl;
import org.dbrain.data.impl.value.MapImpl;
import org.dbrain.data.impl.value.ValueImpl;

import java.io.IOException;

/**
 * Created by epoitras on 08/01/15.
 */
public class JsonValueParser {

    public static JsonToken nextToker( JsonParser parser ) throws IOException {
        return parser.hasCurrentToken() ? parser.getCurrentToken() : parser.nextValue();
    }

    public static Value parseValue( JsonParser parser ) throws IOException, JsonProcessingException {
        JsonToken token = nextToker( parser );
        if ( token != null ) {
            Value result;
            switch ( token ) {
                case VALUE_STRING:
                    result = new ValueImpl( parser.getValueAsString() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NUMBER_FLOAT:
                    result = new ValueImpl( parser.getDoubleValue() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NUMBER_INT:
                    result = new ValueImpl( parser.getBigIntegerValue() );
                    parser.clearCurrentToken();
                    break;
                case VALUE_NULL:
                    result = ValueImpl.NULL;
                    parser.clearCurrentToken();
                    break;
                case VALUE_TRUE:
                    result = ValueImpl.TRUE;
                    parser.clearCurrentToken();
                    break;
                case VALUE_FALSE:
                    result = ValueImpl.FALSE;
                    parser.clearCurrentToken();
                    break;
                case START_OBJECT: {
                    parser.clearCurrentToken();
                    Value.Map values = new MapImpl();
                    while ( nextToker( parser ) != JsonToken.END_OBJECT ) {
                        String key = parser.getCurrentName();
                        Value v = parseValue( parser );
                        if ( v == null ) {
                            throw new JsonParseException( "Expected JSON value.", parser.getCurrentLocation() );
                        }
                        values.put( key, v );
                    }
                    if ( nextToker( parser ) == JsonToken.END_OBJECT ) {
                        parser.clearCurrentToken();
                    } else {
                        throw new JsonParseException( "Expected end of object.", parser.getCurrentLocation() );
                    }
                    result = values;
                }
                break;
                case START_ARRAY: {
                    parser.clearCurrentToken();
                    Value.List values = new ListImpl();
                    while ( nextToker( parser ) != JsonToken.END_ARRAY ) {
                        Value v = parseValue( parser );
                        if ( v == null ) {
                            throw new JsonParseException( "Expected value.", parser.getCurrentLocation() );
                        }
                        values.add( v );
                    }
                    if ( nextToker( parser ) == JsonToken.END_ARRAY ) {
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
