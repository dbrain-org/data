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

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.module.SimpleDeserializers;
import com.fasterxml.jackson.databind.module.SimpleSerializers;
import org.dbrain.data.Value;
import org.dbrain.data.ValueList;
import org.dbrain.data.ValueMap;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Created by epoitras on 07/01/15.
 */
public class JsonModule extends Module {

    @Override
    public String getModuleName() {
        return getClass().getName();
    }

    @Override
    public Version version() {
        return new Version( 1, 0, 0, "", "", "" );
    }

    @Override
    public void setupModule( SetupContext context ) {
        SimpleSerializers serializers = new SimpleSerializers();

        JsonLongSerializer longSerializer = new JsonLongSerializer();

        serializers.addSerializer( Long.class, longSerializer );
        serializers.addSerializer( Long.TYPE, longSerializer );
        serializers.addSerializer( AtomicLong.class, longSerializer );
        serializers.addSerializer( BigDecimal.class, new JsonBigDecimalSerializer() );
        serializers.addSerializer( BigInteger.class, new JsonBigIntegerSerializer() );

        JsonValueSerializer valueSerializer = new JsonValueSerializer();

        serializers.addSerializer( Value.class, valueSerializer );
        serializers.addSerializer( ValueMap.class, valueSerializer );
        serializers.addSerializer( ValueList.class, valueSerializer );

        context.addSerializers( serializers );

        SimpleDeserializers deserializers = new SimpleDeserializers();
        deserializers.addDeserializer( Value.class, new JsonValueDeserializer() );
        deserializers.addDeserializer( ValueMap.class, new JsonValueMapDeserializer() );
        deserializers.addDeserializer( ValueList.class, new JsonValueListDeserializer() );
        context.addDeserializers( deserializers );

    }
}
