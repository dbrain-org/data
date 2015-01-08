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

import com.fasterxml.jackson.databind.ObjectMapper;
import junit.framework.Assert;
import org.dbrain.data.impl.value.json.JsonJsCompatibilityModule;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by epoitras on 06/01/15.
 */
public class JacksonTest {

    ObjectMapper mapper;


    @Before
    public void setUp() throws Exception {
        mapper = new ObjectMapper().registerModule( new JsonJsCompatibilityModule() );

    }

    @Test
    public void testSerializeLong() throws Exception {
        String s = mapper.writeValueAsString( new Long( 10 ) );
        Assert.assertEquals( "\"10\"", s );

    }

    @Test
    public void testSerializeInteger() throws Exception {
        String s = mapper.writeValueAsString( new Integer( 10 ) );
        Assert.assertEquals( "10", s );
    }

    @Test
    public void testSerializeObject() throws Exception {
        String s = mapper.writeValueAsString( new TestLongClass() );
//        Assert.assertEquals(
//                "{\"nativeLong\":\"1\",\"boxedLong\":\"2\",\"nativeInt\":1,\"boxedInt\":2}",
//                s );

        TestLongClass tlc = mapper.readValue( s, TestLongClass.class );
    }
}
