/*
 * Copyright [2014] [Eric Poitras]
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

package org.dbrain.data;

import junit.framework.Assert;
import org.dbrain.data.json.JsonParser;
import org.junit.Test;

import java.io.InputStreamReader;

/**
 * Created by epoitras on 30/06/14.
 */
public class Value_ofJson_Test {

    @Test
    public void test_object_1() throws Exception {
        Value.Map map = Value.ofJson( "{}" ).asMap();
        Assert.assertEquals( 0, map.size() );
    }

    @Test
    public void test_object_2() throws Exception {
        Value.Map map = Value.ofJson( "{titi:test}" ).asMap();
        Assert.assertEquals( 1, map.size() );
        Assert.assertEquals( "test", map.getString( "titi" ) );

    }

    @Test
    public void test_array_1() throws Exception {
        Value.List list = Value.ofJson( "[]" ).asList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        Value.List list = Value.ofJson( "[test, 123, true, null]" ).asList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( 123.0, list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertNull( list.get( 3 ) );
    }

    @Test
    public void testPaseJsonFile() throws Exception {
        Value value = Value.ofJson( new JsonParser( new InputStreamReader( getClass().getResourceAsStream(
                "/SampleJson.json" ) ) ) );

    }
}
