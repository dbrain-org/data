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

import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Created by epoitras on 10/16/15.
 */
public class JacksonSerializerBuilder {

    private final List<Module>                 modules   = new ArrayList<>();
    private final List<Consumer<ObjectMapper>> omConfigs = new ArrayList<>();

    JacksonSerializerBuilder() {
    }


    public JacksonSerializerBuilder withModule( Module module ) {
        if ( module != null ) {
            modules.add( module );
        }
        return this;
    }

    public JacksonSerializerBuilder withModules( Module... modules ) {
        if ( modules != null ) {
            for ( Module m : modules ) {
                withModule( m );
            }
        }
        return this;
    }

    public JacksonSerializerBuilder configureObjectMapper( Consumer<ObjectMapper> omConfig ) {
        if ( omConfig != null ) {
            omConfigs.add( omConfig );
        }
        return this;
    }

    public JacksonSerializer build() {
        ObjectMapper om = new ObjectMapper();

        // Register Modules
        om.registerModules( modules );

        // Customize the Object Mapper
        for ( Consumer<ObjectMapper> omConfig: omConfigs ) {
            omConfig.accept( om );
        }

        return new JacksonSerializer( om );
    }





}
