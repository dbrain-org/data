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

package org.dbrain.data.jackson;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.cfg.MapperConfig;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver;
import com.fasterxml.jackson.databind.jsontype.TypeResolverBuilder;
import com.fasterxml.jackson.databind.jsontype.impl.StdTypeResolverBuilder;
import org.dbrain.data.jackson.artifacts.Person;
import org.junit.Test;

import java.util.Collection;

/**
 * Created by epoitras on 10/27/15.
 */
public class JacksonSerializer_CustomTyping {

    JacksonSerializer serializer = JacksonSerializer.newEmptyBuilder().configureObjectMapper( om -> {
        om.registerSubtypes( Person.class );
        om.setDefaultTyping( new StdTypeResolverBuilder() {
            @Override
            protected TypeIdResolver idResolver( MapperConfig<?> config,
                                                 JavaType baseType,
                                                 Collection<NamedType> subtypes,
                                                 boolean forSer,
                                                 boolean forDeser ) {
                return super.idResolver( config, baseType, subtypes, forSer, forDeser );
            }
        } );
    } ).build();

    @Test
    public void testName() throws Exception {
        serializer.writeToString( new Object() );


    }
}
