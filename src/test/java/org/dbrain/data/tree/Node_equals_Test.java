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

package org.dbrain.data.tree;

import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Test Value methods that are not tested otherwise.
 */
public class Node_equals_Test {

    /**
     * Test the equals on nulls.
     */
    @Test
    public void testNullEquals() throws Exception {

        Assert.assertEquals( Node.of( (Object) null ), Node.of( (Object) null ) );

        Assert.assertNotEquals( Node.of( (Object) null ), Node.of( "" ) );
        Assert.assertNotEquals( Node.of( (Object) null ), Node.of( 123 ) );

    }

    /**
     * Test the equals on booleans.
     */
    @Test
    public void testBooleanEquals() throws Exception {

        Assert.assertEquals( Node.of( true ), Node.of( Boolean.TRUE ) );
        Assert.assertEquals( Node.of( false ), Node.of( Boolean.FALSE ) );

        Assert.assertNotEquals( Node.of( true ), Node.of( "" ) );
        Assert.assertNotEquals( Node.of( true ), Node.of( (Object) null ) );
        Assert.assertNotEquals( Node.of( true ), Node.of( 123 ) );

    }

    /**
     * Test the equals on booleans.
     */
    @Test
    public void testStringEquals() throws Exception {

        Assert.assertEquals( Node.of( "" ), Node.of( "" ) );
        Assert.assertEquals( Node.of( "test" ), Node.of( "test" ) );

        Assert.assertNotEquals( Node.of( "" ), Node.of( " " ) );
        Assert.assertNotEquals( Node.of( "1" ), Node.of( "2" ) );
        Assert.assertNotEquals( Node.of( "" ), Node.of( (Object) null ) );
        Assert.assertNotEquals( Node.of( "" ), Node.of( 123 ) );
        Assert.assertNotEquals( Node.of( "" ), Node.of( false ) );

    }

    /**
     * Test the equals on numbers.
     */
    @Test
    public void testNumberEquals() throws Exception {

        Assert.assertEquals( Node.of( 123 ), Node.of( (byte) 123 ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( (short) 123 ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( 123 ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( (long) 123 ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( 123f ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( 123d ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( new BigDecimal( "123" ) ) );
        Assert.assertEquals( Node.of( 123 ), Node.of( new BigInteger( "123" ) ) );
        Assert.assertEquals( Node.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ),
                             Node.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ) );
        Assert.assertEquals( Node.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ),
                             Node.of( new BigDecimal(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123.00" ) ) );
        Assert.assertEquals( Node.of( new BigDecimal( "0.0" ) ), Node.of( new BigDecimal( "0" ) ) );

        Assert.assertNotEquals( Node.of( 123 ), Node.of( (Object) null ) );
        Assert.assertNotEquals( Node.of( 123 ), Node.of( true ) );
        Assert.assertNotEquals( Node.of( 123 ), Node.of( "123" ) );
        Assert.assertNotEquals( Node.of( 123 ),
                                Node.of( new BigInteger(
                                        "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ) );
        Assert.assertNotEquals( Node.of( 123 ), Node.of( 123.1 ) );

    }

    @Test
    public void testMapEquals() throws Exception {
        NodeMap map1 = NodeMap.newInstance();
        map1.put( "null", Node.of( (Object) null ) );
        map1.put( "int", Node.of( 123 ) );

        NodeMap map2 = NodeMap.newInstance();
        map2.put( "int", Node.of( 123 ) );
        map2.put( "null", Node.of( (Object) null ) );

        NodeMap map3 = NodeMap.newInstance();
        map3.put( "null", Node.of( (Object) null ) );
        map3.put( "int", Node.of( "123" ) );

        Assert.assertEquals( map1, map2 );
        Assert.assertNotEquals( map1, map3 );
        Assert.assertNotEquals( map2, map3 );

    }

    @Test
    public void testListEquals() throws Exception {

        NodeList list1 = NodeList.newBuilder().addNull().add( 123 ).build();

        NodeList list2 = NodeList.newBuilder().addNull().add( 123 ).build();

        NodeList list3 = NodeList.newBuilder().add( 123 ).addNull().build();

        Assert.assertEquals( list1, list2 );
        Assert.assertNotEquals( list1, list3 );
        Assert.assertNotEquals( list2, list3 );

    }
}
