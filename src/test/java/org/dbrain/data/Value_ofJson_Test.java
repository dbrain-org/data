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

import org.dbrain.data.json.JsonBridge;
import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by epoitras on 30/06/14.
 */
public class Value_ofJson_Test {

    @Test
    public void testEmptyStream() throws Exception {
        Assert.assertNull( JsonBridge.get().parseValue( "" ) );
    }

    @Test
    public void test_object_1() throws Exception {
        ValueMap map = JsonBridge.get().parseValue( "{}" ).getMap();
        Assert.assertEquals( 0, map.size() );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_1() throws Exception {
        JsonBridge.get().parseValue( "{" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_2() throws Exception {
        JsonBridge.get().parseValue( "{} {" ).getMap();
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_3() throws Exception {
        JsonBridge.get().parseValue( "{}  [" ).getMap();
    }

    @Test
    public void test_object_2() throws Exception {
        ValueMap map = JsonBridge.get()
                                 .parseValue(
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
        ValueList list = JsonBridge.get().parseValue( "[]" ).getList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        ValueList list = JsonBridge.get().parseValue( "[\"test\", 123, true, null]" ).getList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( new Double( 123.0 ), list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertTrue( list.get( 3 ).isNull() );
    }

    @Test
    public void testPaseJsonFile() throws Exception {
        Value value = JsonBridge.get()
                                .parseValue( new InputStreamReader( getClass().getResourceAsStream( "/SampleJson.json" ) ) );
    }

    @Test
    public void convert_object_to_map_jackson() throws Exception {

        Person test = new Person( "Hey", "bob" );
        test.setFriend( new Person( "Bob", "Marley" ) );
        String s = JsonBridge.get().writeToString( test );

        Value vPerson = JsonBridge.get().parseValue( s );
        System.out.println( vPerson );

        Person test2 = JsonBridge.get().parseObject( s, Person.class );
        Assert.assertEquals( test2.getName(), "Hey" );
        Assert.assertEquals( test2.getLastName(), "bob" );

    }

    public static class Person {

        String name;
        String lastName;
        Person friend;

        public Person() {
        }

        public Person( String name, String lastName ) {
            this.name = name;
            this.lastName = lastName;
        }

        public Person getFriend() {
            return friend;
        }

        public void setFriend( Person friend ) {
            this.friend = friend;
        }

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

}
