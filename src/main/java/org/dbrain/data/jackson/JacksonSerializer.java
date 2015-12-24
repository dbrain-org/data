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
import org.dbrain.data.TextSerializer;
import org.dbrain.data.jackson.modules.StandardModule;
import org.dbrain.data.text.ParseException;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * Reader and writer for Value to JSON format.
 */
public class JacksonSerializer implements TextSerializer {

    /**
     * @return A new builder with already some good defaults setup.
     */
    public static JacksonSerializerBuilder newBuilder() {
        JacksonSerializerBuilder builder = new JacksonSerializerBuilder();
        builder.withModule( new StandardModule() );
        builder.withConfigurator( ( om ) -> om.configure( DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT,
                                                          true ) );
        return builder;
    }

    /**
     * @return A new builder with no default setup.
     */
    public static JacksonSerializerBuilder newEmptyBuilder() {
        return new JacksonSerializerBuilder();
    }

    private final ObjectMapper objectMapper ;

    /**
     * Constructor of the Jackson Serializer from an object mapper.
     */
    public JacksonSerializer( ObjectMapper objectMapper ) {
        this.objectMapper = objectMapper;
    }

    /**
     * Check that there is no more token on the wire.
     */
    private void checkEof( JsonParser parser ) throws IOException {
        JsonToken token = parser.nextToken();
        if ( token != null ) {
            throw new ParseException( "Unexpected json token: "  + token.name() );
        }
    }

    /**
     * Read Object from parser.
     */
    public <T> T read( JsonParser r, Class<T> clazz ) {
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
    public <T> T read( String from, Class<T> clazz ) {
        try {
            JsonParser parser = objectMapper.getFactory().createParser( from );
            T value = read( parser, clazz );
            checkEof( parser );
            return value;
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    /**
     * Read Json from parser.
     */
    @Override
    public <T> T read( Reader from, Class<T> clazz ) {
        try {
            JsonParser parser = objectMapper.getFactory().createParser( from );
            T value = read( parser, clazz );
            checkEof( parser );
            return value;
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

    @Override
    public void write( Writer to, Object o ) {
        try {
            objectMapper.writeValue( to, o );
        } catch ( Exception e ) {
            throw new IllegalStateException( e );
        }
    }

    @Override
    public String writeToString( Object o ) {
        try {
            return objectMapper.writeValueAsString( o );
        } catch ( Exception e ) {
            throw new IllegalStateException( e );
        }
    }

    /**
     * Convert object to value.
     */
    @Override
    public <T> T convert( Object o, Class<T> clazz ) {
        try {
            return objectMapper.convertValue( o, clazz );
        } catch ( Exception e ) {
            throw new ParseException( e );
        }
    }

}
