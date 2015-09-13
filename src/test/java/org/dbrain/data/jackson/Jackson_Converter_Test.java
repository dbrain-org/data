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

import org.dbrain.data.Value;
import org.dbrain.data.ValueList;
import org.dbrain.data.ValueMap;
import org.dbrain.data.jackson.artifacts.MapOfLocaleAsKey;
import org.dbrain.data.jackson.artifacts.Person;
import org.dbrain.data.jackson.artifacts.TestLongClass;
import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

import java.util.Locale;
import java.util.Map;

/**
 * Created by epoitras on 06/01/15.
 */
public class Jackson_Converter_Test {

    JacksonSerializer textSerializer = new JacksonSerializer();


    @Test
    public void testObjectToValue() throws Exception {
        TestLongClass tlc = new TestLongClass();
        ValueMap v = textSerializer.convert( tlc, ValueMap.class );

        Assert.assertNotNull( v );
        Assert.assertEquals( tlc.getBoxedBigDecimal().longValue(), v.getLong( "boxedBigDecimal" ).longValue() );
    }

    @Test
    public void testObjectToValuePrimitive() throws Exception {

        Value v = textSerializer.convert( "Test", Value.class );
        Assert.assertEquals( v, Value.of( "Test" ) );

    }


    /**
     * Test serialization and deserialization of java's Locale.
     */
    @Test
    public void testLocale() throws Exception {
        Locale original = new Locale( "fr", "CA" );

        Value value1 = textSerializer.convert( original, Value.class );
        Locale copy1 = textSerializer.convert( value1, Locale.class );

        String string1 = textSerializer.writeToString( original );
        Locale copy2 = textSerializer.read( string1, Locale.class );

        // Read the json untyped.
        Value untypedValue = textSerializer.read( string1, Value.class );

        Assert.assertEquals( Value.of( "fr_CA" ), untypedValue );
        Assert.assertEquals( original, copy1 );
        Assert.assertEquals( original, copy2 );
    }


    /**
     * Test serialization and deserialization of java's Locale.
     */
    @Test
    public void testLocaleAsKey() throws Exception {
        MapOfLocaleAsKey original = new MapOfLocaleAsKey();
        original.put( new Locale( "fr", "CA" ), "test" );

        Value value1 = textSerializer.convert( original, Value.class );
        MapOfLocaleAsKey copy1 = textSerializer.convert( value1, MapOfLocaleAsKey.class );

        String string1 = textSerializer.writeToString( original );
        MapOfLocaleAsKey copy2 = textSerializer.read( string1, MapOfLocaleAsKey.class );

        // Read the json untyped.
        Value untypedValue = textSerializer.read( string1, Value.class );

        Assert.assertEquals( ValueMap.newBuilder().put( "fr_CA", "test" ).build(), untypedValue );
        Assert.assertEquals( original, copy1 );
        Assert.assertEquals( original, copy2 );
    }


    @Test
    public void testDeserilizeList() throws Exception {
        ValueList list = textSerializer.read( "[1,2,3,4]", ValueList.class );

        Assert.assertEquals( 4, list.size() );
        Assert.assertTrue( list.contains( Value.of( 1 ) ) );
        Assert.assertTrue( list.contains( Value.of( 2 ) ) );
        Assert.assertTrue( list.contains( Value.of( 3 ) ) );
        Assert.assertTrue( list.contains( Value.of( 4 ) ) );
    }

    @Test
    public void testDeserilizeMap() throws Exception {
        ValueMap map = textSerializer.read( "{\"1\":1,\"2\":2,\"3\":3,\"4\":4}", ValueMap.class );

        Assert.assertEquals( 4, map.size() );
        Assert.assertEquals( map.get( "1" ), Value.of( 1 ) );
        Assert.assertEquals( map.get( "2" ), Value.of( 2 ) );
        Assert.assertEquals( map.get( "3" ), Value.of( 3 ) );
        Assert.assertEquals( map.get( "4" ), Value.of( 4 ) );
    }

    @Test
    public void testValueToString() throws Exception {
        ValueMap map = ValueMap.newInstance();
        map.put( "test", Value.of( 123L ) );
        Assert.assertEquals( textSerializer.writeToString( map ), "{\"test\":123}" );
    }

    @Test
    public void convertObjectToMapAndBackAgain() throws Exception {

        Person test = new Person( "Hey", "bob" );
        test.setFriend( new Person( "Bob", "Marley" ) );
        Map m = textSerializer.convert( test, Map.class );

        Person test2 = textSerializer.convert( m, Person.class );
        Assert.assertEquals( test2.getName(), "Hey" );
        Assert.assertEquals( test2.getLastName(), "bob" );

    }

    /**
     * Json does not support circular references. Check that the framework do test it.
     * @throws Exception
     */
    @Test( expected = ParseException.class )
    public void convertObjectToMapCircularReferences() throws Exception {

        Person test = new Person( "Hey", "bob" );
        test.setFriend( test );
        Map m = textSerializer.convert( test, Map.class );

    }


}
