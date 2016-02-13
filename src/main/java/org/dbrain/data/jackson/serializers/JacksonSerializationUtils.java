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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
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

    public static Value parseValue( JsonParser parser, DeserializationContext ctxt  ) throws IOException {
        JsonToken token = getToken( parser );
        if ( token != null ) {
            Value result;
            switch ( token ) {
                case VALUE_STRING:
                    result = Value.of( parser.getValueAsString() );
                    break;
                case VALUE_NUMBER_FLOAT:
                    result = Value.of( parser.getDoubleValue() );
                    break;
                case VALUE_NUMBER_INT:
                    result = Value.of( parser.getBigIntegerValue() );
                    break;
                case VALUE_NULL:
                    result = NullValueImpl.NULL;
                    break;
                case VALUE_TRUE:
                    result = Value.of( Boolean.TRUE );
                    break;
                case VALUE_FALSE:
                    result = Value.of( Boolean.FALSE );
                    break;
                case START_OBJECT: {
                    ValueMap values = ValueMap.newInstance();
                    while ( parser.nextToken() == JsonToken.FIELD_NAME ) {
                        String key = parser.getCurrentName();
                        parser.nextToken();
                        Value v = parseValue( parser, ctxt );
                        if ( v == null ) {
                            throw ctxt.wrongTokenException( parser, JsonToken.START_OBJECT, "Expected Value" );
                        }
                        values.put( key, v );
                    }
                    if ( getToken( parser ) == JsonToken.END_OBJECT ) {
                        parser.clearCurrentToken();
                    } else {
                        throw ctxt.wrongTokenException( parser, JsonToken.END_OBJECT, null );
                    }
                    result = values;
                }
                break;
                case START_ARRAY: {
                    ValueList values = ValueList.newInstance();
                    while ( parser.nextToken() != JsonToken.END_ARRAY ) {
                        Value v = parseValue( parser, ctxt );
                        if ( v == null ) {
                            throw ctxt.wrongTokenException( parser, JsonToken.START_OBJECT, "Expected Value" );
                        }
                        values.add( v );
                    }
                    if ( getToken( parser ) == JsonToken.END_ARRAY ) {
                        parser.clearCurrentToken();
                    } else {
                        throw ctxt.wrongTokenException( parser, JsonToken.END_ARRAY, null );
                    }
                    result = values;
                }
                break;
                default:
                    throw ctxt.wrongTokenException( parser, JsonToken.START_OBJECT, "Expected Value" );
            }
            return result;
        } else {
            return null;
        }
    }
}
