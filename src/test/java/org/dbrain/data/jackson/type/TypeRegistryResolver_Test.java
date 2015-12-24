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

package org.dbrain.data.jackson.type;

import com.fasterxml.jackson.databind.SerializationFeature;
import org.dbrain.data.TypeRegistry;
import org.dbrain.data.jackson.JacksonSerializer;
import org.dbrain.data.jackson.type.artifacts.Ext1;
import org.dbrain.data.jackson.type.artifacts.Ext2;
import org.dbrain.data.jackson.type.artifacts.Root;
import org.junit.Assert;
import org.junit.Test;

import java.util.Map;

/**
 * Created by epoitras on 12/23/15.
 */
public class TypeRegistryResolver_Test {

    @Test
    public void testCustomTypes() throws Exception {
        TypeRegistry<Root> rootTypeRegistry = TypeRegistry.from( Root.class, c -> c.getSimpleName() )
                                                          .registerType( Root.class )
                                                          .registerType( Ext1.class )
                                                          .registerType( Ext2.class )
                                                          .build();
        TypeRegistryResolver typing = TypeRegistryResolver.of( rootTypeRegistry, "@type" );
        JacksonSerializer serializer = JacksonSerializer
                .newBuilder()
                .withTyping( typing )
                .withConfigurator( om -> om.disable( SerializationFeature.FAIL_ON_EMPTY_BEANS ) )
                .build();

        Map object = serializer.convert( new Ext1( "test1" ), Map.class );
        Map object2 = serializer.convert( new Ext2( ), Map.class );
        String typeInfo = (String) object.get( "@type" );
        Assert.assertEquals( typeInfo, "Ext1" );

        Ext1 ext1 = serializer.convert( object, Ext1.class );
        Assert.assertNotNull( ext1 );

    }
}