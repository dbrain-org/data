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

import com.fasterxml.jackson.databind.ObjectMapper;
import junit.framework.Assert;
import org.dbrain.data.impl.value.MapImpl;
import org.dbrain.data.text.ParseException;
import org.junit.Test;

import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by epoitras on 30/06/14.
 */
public class Value_ofJson_Test {

    @Test
    public void testEmptyStream() throws Exception {
        Assert.assertNull( Value.ofJson( "" ) );
    }

    @Test
    public void test_object_1() throws Exception {
        Value.Map map = Value.ofJson( "{}" ).getMap();
        Assert.assertEquals( 0, map.size() );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_1() throws Exception {
        Value.ofJson( "{" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_2() throws Exception {
        Value.ofJson( "{} {" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_3() throws Exception {
        Value.ofJson( "{}  [" ).getMap();
    }

    @Test
    public void test_object_2() throws Exception {
        Value.Map map = Value.ofJson(
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
        Value.List list = Value.ofJson( "[]" ).getList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        Value.List list = Value.ofJson( "[\"test\", 123, true, null]" ).getList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( 123.0, list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertTrue( list.get( 3 ).isNull() );
    }

    @Test
    public void testPaseJsonFile() throws Exception {
        Value value = Value.ofJson( new InputStreamReader( getClass().getResourceAsStream( "/SampleJson.json" ) ) );
    }

    public static class Person {

        public Person( String name, String lastName ) {
            this.name = name;
            this.lastName = lastName;
        }

        String name;

        String lastName;

        public Person getFriend() {
            return friend;
        }

        public void setFriend( Person friend ) {
            this.friend = friend;
        }

        Person friend;

        public String getName() {
            return name;
        }

        public void setName( String name ) {
            this.name = name;
        }

        public String getLastName() {
            return lastName;
        }

        public void setLastName( String lastName ) {
            this.lastName = lastName;
        }
    }

    @Test
    public void convert_object_to_map_jackson() throws Exception {

        ObjectMapper objectMapper = new ObjectMapper();

        StringWriter sw = new StringWriter(  );

        Person test = new Person( "Hey", "bob" );
        test.setFriend( new Person( "Bob", "Marley" ) );
        objectMapper.writeValue( sw, test );

        Value vPerson = Value.ofJson( sw.toString() );
        System.out.println( vPerson );

    }

}
