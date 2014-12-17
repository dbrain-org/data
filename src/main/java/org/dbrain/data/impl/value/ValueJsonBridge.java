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

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonLocation;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import org.dbrain.data.Value;
import org.dbrain.data.text.ParseException;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;

/**
 * Reader and writer for Value to JSON format.
 */
public class ValueJsonBridge {

    private static JsonFactory jsonFactory = new JsonFactory(); // or, for data binding, org.codehaus.jackson.mapper.MappingJsonFactory

    private static JsonToken token( JsonParser parser ) throws IOException {
        return parser.hasCurrentToken() ? parser.getCurrentToken() : parser.nextValue();
    }

    private static ParseException error( JsonParser parser, String message, Throwable e ) {
        StringWriter sw = new StringWriter();
        PrintWriter printWriter = new PrintWriter( sw );
        if ( message != null ) {
            printWriter.println( message );
        }
        if ( parser != null ) {
            JsonLocation loc = parser.getCurrentLocation();
            if ( loc != null ) {
                printWriter.println( String.format( "At line %d, column %d, offset(%d)",
                                                    loc.getLineNr(),
                                                    loc.getColumnNr(),
                                                    loc.getCharOffset() ) );

                printWriter.println( String.format( "Source: %s", loc.getSourceRef() ) );
            }
        }

        return new ParseException( sw.toString(), e );
    }

    private static ParseException error( JsonParser parser, String message ) {
        return error( parser, message, null );
    }

    public static Value ofJson( Reader r ) {
        try {
            JsonParser p = jsonFactory.createParser( r );
            Value v = ofJson( p );
            if ( token( p ) != null ) {
                throw error( p, "Expected end of file." );
            }
            return v;
        } catch ( IOException e ) {
            throw new ParseException( e );
        }
    }

    public static Value ofJson( String r ) {
        try {
            JsonParser p = jsonFactory.createParser( r );
            Value v = ofJson( p );
            if ( token( p ) != null ) {
                throw error( p, "Expected end of string." );
            }
            return v;
        } catch ( IOException e ) {
            throw new ParseException( e );
        }
    }

    public static Value ofJson( JsonParser parser ) {
        try {
            JsonToken token = token( parser );
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
                        while ( token( parser ) != JsonToken.END_OBJECT ) {
                            String key = parser.getCurrentName();
                            Value v = ofJson( parser );
                            if ( v == null ) {
                                throw error( parser, "Expected JSON value." );
                            }
                            values.put( key, v );
                        }
                        if ( token( parser ) == JsonToken.END_OBJECT ) {
                            parser.clearCurrentToken();
                        } else {
                            throw error( parser, "Expected end of object." );
                        }
                        result = values;
                    }
                    break;
                    case START_ARRAY: {
                        parser.clearCurrentToken();
                        Value.List values = new ListImpl();
                        while ( token( parser ) != JsonToken.END_ARRAY ) {
                            Value v = ofJson( parser );
                            if ( v == null ) {
                                throw error( parser, "Expected value." );
                            }
                            values.add( v );
                        }
                        if ( token( parser ) == JsonToken.END_ARRAY ) {
                            parser.clearCurrentToken();
                        } else {
                            throw error( parser, "Expected end of array." );
                        }
                        result = values;
                    }
                    break;
                    default:
                        throw error( parser, "Expected value." );
                }
                return result;
            } else {
                return null;
            }
        } catch ( IOException e ) {
            throw error( parser, "IO error while parsing.", e );
        }
    }

}
