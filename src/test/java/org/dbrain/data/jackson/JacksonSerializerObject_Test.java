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

package org.dbrain.data.jackson;

import org.dbrain.data.Serializer;
import org.dbrain.data.Value;
import org.dbrain.data.ValueList;
import org.dbrain.data.ValueMap;
import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by epoitras on 30/06/14.
 */
public class JacksonSerializerObject_Test {

    Serializer serializer = new JacksonJsonSerializer();

    @Test
    public void testEmptyStream() throws Exception {
        Assert.assertNull( serializer.parseValue( "" ) );
    }

    @Test
    public void test_object_1() throws Exception {
        ValueMap map = serializer.parseValue( "{}" ).getMap();
        Assert.assertEquals( 0, map.size() );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_1() throws Exception {
        serializer.parseValue( "{" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_2() throws Exception {
        serializer.parseValue( "{} {" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_3() throws Exception {
        serializer.parseValue( "{}  [" ).getMap();
    }

    @Test
    public void test_object_2() throws Exception {
        ValueMap map = serializer.parseValue(
                "{ \"boolean_true\" : true, \"boolean_false\" : false, \"null\": null, \"string\": \"string\", \"double\":123.4, \"integer\":123456789,\"array\":[],\"object\":{} }" )
                                 .getMap();
        Assert.assertEquals( 8, map.size() );
        Assert.assertEquals( Boolean.TRUE.toString(), map.getString( "boolean_true" ) );
        Assert.assertEquals( Boolean.FALSE.toString(), map.getString( "boolean_false" ) );
        Assert.assertNull( map.getObject( "null" ) );
        Assert.assertEquals( "string", map.getString( "string" ) );
        Assert.assertEquals( new Double( 123.4D ), map.getDouble( "double" ) );
        Assert.assertEquals( new Integer( 123456789 ), map.getInt( "integer" ) );
        Assert.assertEquals( new ArrayList<>(), map.getObject( "array" ) );
        Assert.assertEquals( new HashMap<>(), map.getObject( "object" ) );
    }

    @Test
    public void test_array_1() throws Exception {
        ValueList list = serializer.parseValue( "[]" ).getList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        ValueList list = serializer.parseValue( "[\"test\", 123, true, null]" ).getList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( new Double( 123.0 ), list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertTrue( list.get( 3 ).isNull() );
    }

    @Test
    public void testPaseJsonFile() throws Exception {
        Value value = serializer
                                .parseValue( new InputStreamReader( getClass().getResourceAsStream( "/SampleJson.json" ) ) );
    }


}
