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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.dbrain.data.Value;
import org.dbrain.data.ValueList;
import org.dbrain.data.ValueMap;

import java.io.IOException;
import java.util.Map;

/**
 * Created by epoitras on 08/01/15.
 */
public class JsonValueSerializer extends JsonSerializer<Value> {

    private void writeMap( ValueMap value, JsonGenerator w ) throws IOException {
        w.writeStartObject();
        try {
            for ( Map.Entry<String, Value> e : value.entrySet() ) {
                w.writeFieldName( e.getKey() );
                writeValue( e.getValue(), w );
            }
        } finally {
            w.writeEndObject();
        }
    }

    private void writeList( ValueList value, JsonGenerator w ) throws IOException {
        w.writeStartArray();
        try {
            for ( Value e : value ) {
                writeValue( e, w );
            }
        } finally {
            w.writeEndObject();
        }
    }

    /**
     * Write a value to a generator.
     */
    public void writeValue( Value value, JsonGenerator w ) throws IOException {
        if ( value == null || value.isNull() ) {
            w.writeNull();
        } else if ( value instanceof ValueMap ) {
            writeMap( (ValueMap) value, w );
        } else if ( value instanceof ValueList ) {
            writeList( (ValueList) value, w );
        } else {
            w.writeObject( value.getObject() );
        }
    }


    @Override
    public void serialize( Value value,
                           JsonGenerator jgen,
                           SerializerProvider provider ) throws IOException, JsonProcessingException {
        writeValue( value, jgen );
    }
}
