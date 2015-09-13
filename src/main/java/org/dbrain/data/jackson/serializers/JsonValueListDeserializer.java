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
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.dbrain.data.ValueList;

import java.io.IOException;

/**
 * Parse a Json value.
 */
public class JsonValueListDeserializer extends JsonDeserializer<ValueList> {

    @Override
    public ValueList deserialize( JsonParser jp, DeserializationContext ctxt ) throws IOException {
        return JacksonSerializationUtils.parseValue( jp, ctxt ).getList();
    }
}
