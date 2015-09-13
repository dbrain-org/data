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
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Serialize BigDecimal with more than 15 significant digits to String literal.
 */
public class JsonBigDecimalSerializer extends JsonSerializer<BigDecimal> {

    private BigInteger MAX_VALUE = new BigInteger( "999999999999999" );
    private BigInteger MIN_VALUE = new BigInteger( "-999999999999999" );

    @Override
    public void serialize( BigDecimal value, JsonGenerator jgen, SerializerProvider provider ) throws IOException {
        if ( value != null ) {
            // Does it have more that 15 significant digits ?
            BigInteger unscaled = value.unscaledValue();
            if ( unscaled.compareTo( MIN_VALUE ) >= 0 && unscaled.compareTo( MAX_VALUE ) <= 0 ) {
                jgen.writeNumber( value );
            } else {
                jgen.writeString( value.toString() );
            }
        } else {
            jgen.writeNull();
        }
    }

}
