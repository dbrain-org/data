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

import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;
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
public class JacksonSerializer_Converter_Test {

    JacksonSerializer textSerializer = JacksonSerializer.newBuilder().build();

    @Test
    public void testObjectToValue() throws Exception {
        TestLongClass tlc = new TestLongClass();
        NodeMap v = textSerializer.convert( tlc, NodeMap.class );

        Assert.assertNotNull( v );
        Assert.assertEquals( tlc.getBoxedBigDecimal().longValue(), v.getLong( "boxedBigDecimal" ).longValue() );
    }

    @Test
    public void testObjectToValuePrimitive() throws Exception {

        Node v = textSerializer.convert( "Test", Node.class );
        Assert.assertEquals( v, Node.of( "Test" ) );

    }


    /**
     * Test serialization and deserialization of java's Locale.
     */
    @Test
    public void testLocale() throws Exception {
        Locale original = new Locale( "fr", "CA" );

        Node node1 = textSerializer.convert( original, Node.class );
        Locale copy1 = textSerializer.convert(node1, Locale.class );

        String string1 = textSerializer.writeToString( original );
        Locale copy2 = textSerializer.read( string1, Locale.class );

        // Read the json untyped.
        Node untypedNode = textSerializer.read( string1, Node.class );

        Assert.assertEquals( Node.of( "fr_CA" ), untypedNode);
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

        Node node1 = textSerializer.convert( original, Node.class );
        MapOfLocaleAsKey copy1 = textSerializer.convert(node1, MapOfLocaleAsKey.class );

        String string1 = textSerializer.writeToString( original );
        MapOfLocaleAsKey copy2 = textSerializer.read( string1, MapOfLocaleAsKey.class );

        // Read the json untyped.
        Node untypedNode = textSerializer.read( string1, Node.class );

        Assert.assertEquals( NodeMap.newBuilder().put( "fr_CA", "test" ).build(), untypedNode);
        Assert.assertEquals( original, copy1 );
        Assert.assertEquals( original, copy2 );
    }


    @Test
    public void testDeserilizeList() throws Exception {
        NodeList list = textSerializer.read( "[1,2,3,4]", NodeList.class );

        Assert.assertEquals( 4, list.size() );
        Assert.assertTrue( list.contains( Node.of( 1 ) ) );
        Assert.assertTrue( list.contains( Node.of( 2 ) ) );
        Assert.assertTrue( list.contains( Node.of( 3 ) ) );
        Assert.assertTrue( list.contains( Node.of( 4 ) ) );
    }

    @Test
    public void testDeserilizeMap() throws Exception {
        NodeMap map = textSerializer.read( "{\"1\":1,\"2\":2,\"3\":3,\"4\":4}", NodeMap.class );

        Assert.assertEquals( 4, map.size() );
        Assert.assertEquals( map.get( "1" ), Node.of( 1 ) );
        Assert.assertEquals( map.get( "2" ), Node.of( 2 ) );
        Assert.assertEquals( map.get( "3" ), Node.of( 3 ) );
        Assert.assertEquals( map.get( "4" ), Node.of( 4 ) );
    }

    @Test
    public void testValueToString() throws Exception {
        NodeMap map = NodeMap.newInstance();
        map.put( "test", Node.of( 123L ) );
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
