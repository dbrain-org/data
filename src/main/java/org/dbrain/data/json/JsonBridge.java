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

import com.fasterxml.jackson.core.JsonParser;
import org.dbrain.data.Value;
import org.dbrain.data.impl.json.jackson.JacksonJsonBridge;

import java.io.Reader;

/**
 * Created by epoitras on 16/01/15.
 */
public interface JsonBridge {

    static JsonBridge INSTANCE = new JacksonJsonBridge();

    static JsonBridge get() {
        return INSTANCE;
    }

    Value parseValue( String r );

    Value parseValue( Reader r );

    <T> T parseObject( String r, Class<T> clazz );

    Value objectToValue( Object o );

    String writeToString( Object o );
}
