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

import org.dbrain.data.Serializer;
import org.dbrain.data.Value;
import org.dbrain.data.jackson.artifacts.Person;
import org.dbrain.data.jackson.artifacts.TestLongClass;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;

/**
 * Created by epoitras on 30/06/14.
 */
public class JacksonSerializer_ObjectToString_Test {

    Serializer serializer = new JacksonJsonSerializer();

    @Test
    public void testObjectToString() throws Exception {
        String s1 = serializer.objectToString( new Long( 10 ) );
        String s2 = serializer.objectToString( new Long( 999999999999999l ) );
        String s3 = serializer.objectToString( new Long( 1000000000000000l ) );
        String s4 = serializer.objectToString( new Long( -999999999999999l ) );
        String s5 = serializer.objectToString( new Long( -1000000000000000l ) );

        Assert.assertEquals( "10", s1 );
        Assert.assertEquals( "999999999999999", s2 );
        Assert.assertEquals( "\"1000000000000000\"", s3 );
        Assert.assertEquals( "-999999999999999", s4 );
        Assert.assertEquals( "\"-1000000000000000\"", s5 );

    }

    @Test
    public void testSerializeBigDecimal() throws Exception {
        String s1 = serializer.objectToString( new BigDecimal( "10" ) );
        String s2 = serializer.objectToString( new BigDecimal( "999999999999999" ) );
        String s3 = serializer.objectToString( new BigDecimal( "1000000000000000" ) );
        String s4 = serializer.objectToString( new BigDecimal( "99999999999.9999" ) );
        String s5 = serializer.objectToString( new BigDecimal( "100000000000.1234" ) );
        String s6 = serializer.objectToString( new BigDecimal( "-999999999999999" ) );
        String s7 = serializer.objectToString( new BigDecimal( "-1000000000000000" ) );
        String s8 = serializer.objectToString( new BigDecimal( "-99999999999.9999" ) );
        String s9 = serializer.objectToString( new BigDecimal( "-100000000000.1234" ) );

        Assert.assertEquals( "10", s1 );
        Assert.assertEquals( "999999999999999", s2 );
        Assert.assertEquals( "\"1000000000000000\"", s3 );
        Assert.assertEquals( "99999999999.9999", s4 );
        Assert.assertEquals( "\"100000000000.1234\"", s5 );
        Assert.assertEquals( "-999999999999999", s6 );
        Assert.assertEquals( "\"-1000000000000000\"", s7 );
        Assert.assertEquals( "-99999999999.9999", s8 );
        Assert.assertEquals( "\"-100000000000.1234\"", s9 );

    }


    @Test
    public void testSerializeInteger() throws Exception {
        String s = serializer.objectToString( new Integer( 10 ) );
        Assert.assertEquals( "10", s );
    }

    @Test
    public void testSerializeObject() throws Exception {
        String s = serializer.objectToString( new TestLongClass() );
        TestLongClass tlc = serializer.parseObject( s, TestLongClass.class );
        Assert.assertNotNull( tlc );
    }


    @Test
    public void convert_object_to_map_jackson() throws Exception {

        Person test = new Person( "Hey", "bob" );
        test.setFriend( new Person( "Bob", "Marley" ) );
        String s = serializer.objectToString( test );

        Value vPerson = serializer.parseValue( s );
        System.out.println( vPerson );

        Person test2 = serializer.parseObject( s, Person.class );
        Assert.assertEquals( test2.getName(), "Hey" );
        Assert.assertEquals( test2.getLastName(), "bob" );

    }

}
