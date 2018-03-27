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

package org.dbrain.data.jackson.modules;

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.module.SimpleDeserializers;
import com.fasterxml.jackson.databind.module.SimpleSerializers;
import org.dbrain.data.Fqn;
import org.dbrain.data.Path;
import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;
import org.dbrain.data.jackson.serializers.FqnDeserializer;
import org.dbrain.data.jackson.serializers.FqnSerializer;
import org.dbrain.data.jackson.serializers.JsonBigDecimalSerializer;
import org.dbrain.data.jackson.serializers.JsonBigIntegerSerializer;
import org.dbrain.data.jackson.serializers.JsonLongSerializer;
import org.dbrain.data.jackson.serializers.JsonValueDeserializer;
import org.dbrain.data.jackson.serializers.JsonValueListDeserializer;
import org.dbrain.data.jackson.serializers.JsonValueMapDeserializer;
import org.dbrain.data.jackson.serializers.JsonValueSerializer;
import org.dbrain.data.jackson.serializers.PathDeserializer;
import org.dbrain.data.jackson.serializers.PathSerializer;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Module to register standard serializer.
 */
public class StandardModule extends Module {

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
        serializers.addSerializer( Path.class, new PathSerializer() );
        serializers.addSerializer( Fqn.class, new FqnSerializer() );
        serializers.addSerializer( Node.class, new JsonValueSerializer() );
        context.addSerializers( serializers );

        SimpleDeserializers deserializers = new SimpleDeserializers();
        deserializers.addDeserializer( Node.class, new JsonValueDeserializer() );
        deserializers.addDeserializer( NodeMap.class, new JsonValueMapDeserializer() );
        deserializers.addDeserializer( NodeList.class, new JsonValueListDeserializer() );
        deserializers.addDeserializer( Path.class, new PathDeserializer() );
        deserializers.addDeserializer( Fqn.class, new FqnDeserializer() );
        context.addDeserializers( deserializers );

    }
}
