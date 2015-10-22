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

import org.dbrain.data.Path;
import org.dbrain.data.TextSerializer;
import org.dbrain.data.Value;
import org.dbrain.data.jackson.artifacts.Person;
import org.dbrain.data.jackson.artifacts.TestLongClass;
import org.junit.Assert;
import org.junit.Test;

import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Created by epoitras on 30/06/14.
 */
public class JacksonSerializer_Codec_Test {

    TextSerializer    standardSerializer = JacksonSerializer.newBuilder().build();
    JacksonSerializer defaultSerializer  = JacksonSerializer.newEmptyBuilder().build();

    @Test
    public void testWrite() throws Exception {
        StringWriter sw = new StringWriter();

        standardSerializer.write( sw, new Long( 10 ) );
        Assert.assertEquals( "10", sw.toString() );

    }

    @Test
    public void testPath() throws Exception {
        Path p = Path.of( "testpath" );

        String w = standardSerializer.writeToString( p );
        Path p2 = standardSerializer.read( w, Path.class );

        Assert.assertEquals( p, p2 );

    }


    @Test
    public void testLongSpecialEncoding() throws Exception {
        String s1 = standardSerializer.writeToString( new Long( 10 ) );
        String s2 = standardSerializer.writeToString( new Long( 999999999999999l ) );
        String s3 = standardSerializer.writeToString( new Long( 1000000000000000l ) );
        String s4 = standardSerializer.writeToString( new Long( -999999999999999l ) );
        String s5 = standardSerializer.writeToString( new Long( -1000000000000000l ) );

        Assert.assertEquals( "10", s1 );
        Assert.assertEquals( "999999999999999", s2 );
        Assert.assertEquals( "\"1000000000000000\"", s3 );
        Assert.assertEquals( "-999999999999999", s4 );
        Assert.assertEquals( "\"-1000000000000000\"", s5 );

    }

    @Test
    public void testSerializeBigDecimal() throws Exception {
        String s1 = standardSerializer.writeToString( new BigDecimal( "10" ) );
        String s2 = standardSerializer.writeToString( new BigDecimal( "999999999999999" ) );
        String s3 = standardSerializer.writeToString( new BigDecimal( "1000000000000000" ) );
        String s4 = standardSerializer.writeToString( new BigDecimal( "99999999999.9999" ) );
        String s5 = standardSerializer.writeToString( new BigDecimal( "100000000000.1234" ) );
        String s6 = standardSerializer.writeToString( new BigDecimal( "-999999999999999" ) );
        String s7 = standardSerializer.writeToString( new BigDecimal( "-1000000000000000" ) );
        String s8 = standardSerializer.writeToString( new BigDecimal( "-99999999999.9999" ) );
        String s9 = standardSerializer.writeToString( new BigDecimal( "-100000000000.1234" ) );

        Assert.assertEquals( "10", s1 );
        Assert.assertEquals( "999999999999999", s2 );
        Assert.assertEquals( "\"1000000000000000\"", s3 );
        Assert.assertEquals( "99999999999.9999", s4 );
        Assert.assertEquals( "\"100000000000.1234\"", s5 );
        Assert.assertEquals( "-999999999999999", s6 );
        Assert.assertEquals( "\"-1000000000000000\"", s7 );
        Assert.assertEquals( "-99999999999.9999", s8 );
        Assert.assertEquals( "\"-100000000000.1234\"", s9 );

        // Compare to default config
        String d1 = defaultSerializer.writeToString( new BigDecimal( "10" ) );
        String d2 = defaultSerializer.writeToString( new BigDecimal( "999999999999999" ) );
        String d3 = defaultSerializer.writeToString( new BigDecimal( "1000000000000000" ) );
        String d4 = defaultSerializer.writeToString( new BigDecimal( "99999999999.9999" ) );
        String d5 = defaultSerializer.writeToString( new BigDecimal( "100000000000.1234" ) );
        String d6 = defaultSerializer.writeToString( new BigDecimal( "-999999999999999" ) );
        String d7 = defaultSerializer.writeToString( new BigDecimal( "-1000000000000000" ) );
        String d8 = defaultSerializer.writeToString( new BigDecimal( "-99999999999.9999" ) );
        String d9 = defaultSerializer.writeToString( new BigDecimal( "-100000000000.1234" ) );

        Assert.assertEquals( "10", d1 );
        Assert.assertEquals( "999999999999999", d2 );
        Assert.assertEquals( "1000000000000000", d3 );
        Assert.assertEquals( "99999999999.9999", d4 );
        Assert.assertEquals( "100000000000.1234", d5 );
        Assert.assertEquals( "-999999999999999", d6 );
        Assert.assertEquals( "-1000000000000000", d7 );
        Assert.assertEquals( "-99999999999.9999", d8 );
        Assert.assertEquals( "-100000000000.1234", d9 );

    }

    @Test
    public void testSerializeInteger() throws Exception {
        String s = standardSerializer.writeToString( new Integer( 10 ) );
        Assert.assertEquals( "10", s );
    }

    @Test
    public void testSerializeObject() throws Exception {
        String s = standardSerializer.writeToString( new TestLongClass() );
        TestLongClass tlc = standardSerializer.read( s, TestLongClass.class );
        Assert.assertNotNull( tlc );
    }


    @Test
    public void convert_object_to_map_jackson() throws Exception {

        Person test = new Person( "Hey", "bob" );
        test.setFriend( new Person( "Bob", "Marley" ) );
        String s = standardSerializer.writeToString( test );

        Value vPerson = standardSerializer.read( s, Value.class );
        System.out.println( vPerson );

        Person test2 = standardSerializer.read( s, Person.class );
        Assert.assertEquals( test2.getName(), "Hey" );
        Assert.assertEquals( test2.getLastName(), "bob" );

    }

}
