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

package org.dbrain.data.impl.json.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;

/**
 * Serialize long with more that 15 digits to String literal.
 *
 * This class is generic "number" as we want to use it for AtomicLong as well.
 */
class JsonLongSerializer extends JsonSerializer<Number> {

    private long MAX_VALUE = 999999999999999l;
    private long MIN_VALUE = -999999999999999l;

    @Override
    public void serialize( Number value,
                           JsonGenerator jgen,
                           SerializerProvider provider ) throws IOException {
        if ( value != null ) {
            long longValue = value.longValue();
            if ( longValue >= MIN_VALUE && longValue <= MAX_VALUE ) {
                jgen.writeNumber( longValue );
            } else {
                jgen.writeString( value.toString() );
            }
        } else {
            jgen.writeNull();
        }
    }

}
