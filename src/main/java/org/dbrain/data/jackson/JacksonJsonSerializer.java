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

package org.dbrain.data.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.dbrain.data.Value;
import org.dbrain.data.Serializer;
import org.dbrain.data.text.ParseException;

import java.io.IOException;
import java.io.Reader;

/**
 * Reader and writer for Value to JSON format.
 */
public class JacksonJsonSerializer implements Serializer {

    private ObjectMapper objectMapper = new ObjectMapper() //
            .configure( DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true ) //
            .registerModule( new JsonModule() );

    /**
     * Check that there is no more token on the wire.
     */
    private void checkEof( JsonParser parser ) throws IOException {
        JsonToken token = parser.nextToken();
        if ( parser.getCurrentToken() != null ) {
            throw new ParseException( "Unexpected json token: " + token.name() );
        }
    }

    /**
     * Parse a value from string.
     */
    @Override
    public Value parseValue( String r ) {
        try {
            JsonParser parser = objectMapper.getFactory().createParser( r );
            Value v = parseValue( parser );
            checkEof( parser );
            return v;
        } catch ( IOException e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Parse a value from a reader.
     */
    @Override
    public Value parseValue( Reader r ) {
        try {
            try ( Reader r2 = r ) {
                JsonParser parser = objectMapper.getFactory().createParser( r2 );
                Value v = parseValue( parser );
                checkEof( parser );
                return v;
            }
        } catch ( IOException e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Parse a value from a json parser.
     */
    public Value parseValue( JsonParser r ) {
        try {
            return JsonValueParser.parseValue( r );
        } catch ( IOException e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Read Object from parser.
     */
    public <T> T parseObject( JsonParser r, Class<T> clazz ) {
        try {
            return objectMapper.readValue( r, clazz );
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Read Json from parser.
     */
    @Override
    public <T> T parseObject( String r, Class<T> clazz ) {
        try {
            JsonParser parser = objectMapper.getFactory().createParser( r );
            T value = parseObject( parser, clazz );
            checkEof( parser );
            return value;
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    @Override
    public <T> T parseObject( Value v, Class<T> clazz ) {
        try {
            return objectMapper.convertValue( v, clazz );
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Convert object to value.
     */
    @Override
    public Value objectToValue( Object o ) {
        try {
            return objectMapper.convertValue( o, Value.class );
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Convert an object to Json String.
     */
    @Override
    public String objectToString( Object o ) {
        try {
            return objectMapper.writeValueAsString( o );
        } catch ( Exception e ) {
            throw new IllegalStateException( e );
        }
    }

}
