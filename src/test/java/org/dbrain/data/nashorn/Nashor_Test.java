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

package org.dbrain.data.nashorn;

import jdk.nashorn.api.scripting.JSObject;
import jdk.nashorn.api.scripting.ScriptObjectMirror;
import org.dbrain.data.Value;
import org.dbrain.data.ValueMap;
import org.junit.Test;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleBindings;
import java.util.Map;

/**
 * Created by epoitras on 2/19/15.
 */
public class Nashor_Test {

    @Test
    public void testJsonObject() throws Exception {
        ScriptEngineManager factory = new ScriptEngineManager();
        ScriptEngine scriptEngine = factory.getEngineByName( "nashorn" );

        long t = System.currentTimeMillis();
        JSObject o1 = (JSObject) scriptEngine.eval( "function( r ) { print( r.int ); }" );

        ValueMap vm = ValueMap.newBuilder()
                              .put( "int", 123 )
                              .put( "str", "test" )
                              .put( "", 123 )
                              .build();

        Bindings sb = scriptEngine.createBindings();
        sb.putAll( vm );

        for ( int i = 0; i < 10; i++ ) {

            o1.call( null, sb );
            ///Value v = ValueMap.of( (Map) o );
        }
        System.out.println( System.currentTimeMillis() - t );


    }
}
