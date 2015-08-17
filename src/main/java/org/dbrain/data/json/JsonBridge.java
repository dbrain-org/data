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

package org.dbrain.data.json;

import org.dbrain.data.Value;
import org.dbrain.data.impl.json.jackson.JacksonJsonBridge;

import java.io.Reader;

/**
 * Simple bridge to JSON format.
 */
public interface JsonBridge {

    JsonBridge INSTANCE = new JacksonJsonBridge();

    static JsonBridge get() {
        return INSTANCE;
    }

    /**
     * Parse a JSON String to Value.
     */
    Value parseValue( String r );

    /**
     * Parse a JSON stream to Value.
     */
    Value parseValue( Reader r );

    /**
     * Parse a JSON String to the specific class.
     */
    <T> T parseObject( String r, Class<T> clazz );

    /**
     * Parse a Value to the specific class.
     */
    <T> T parseObject( Value v, Class<T> clazz );

    /**
     * Stream object to Value.
     */
    Value objectToValue( Object o );

    /**
     * Stream Object to String.
     */
    String objectToString( Object o );
}
