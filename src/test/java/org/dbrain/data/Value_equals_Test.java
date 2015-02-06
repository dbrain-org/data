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

package org.dbrain.data;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Test Value methods that are not tested otherwise.
 */
public class Value_equals_Test {

    /**
     * Test the equals on nulls.
     */
    @Test
    public void testNullEquals() throws Exception {

        Assert.assertEquals( Value.of( (Object) null ), Value.of( (Object) null ) );

        Assert.assertNotEquals( Value.of( (Object) null ), Value.of( "" ) );
        Assert.assertNotEquals( Value.of( (Object) null ), Value.of( 123 ) );

    }

    /**
     * Test the equals on booleans.
     */
    @Test
    public void testBooleanEquals() throws Exception {

        Assert.assertEquals( Value.of( true ), Value.of( Boolean.TRUE ) );
        Assert.assertEquals( Value.of( false ), Value.of( Boolean.FALSE ) );

        Assert.assertNotEquals( Value.of( true ), Value.of( "" ) );
        Assert.assertNotEquals( Value.of( true ), Value.of( (Object) null ) );
        Assert.assertNotEquals( Value.of( true ), Value.of( 123 ) );

    }

    /**
     * Test the equals on booleans.
     */
    @Test
    public void testStringEquals() throws Exception {

        Assert.assertEquals( Value.of( "" ), Value.of( "" ) );
        Assert.assertEquals( Value.of( "test" ), Value.of( "test" ) );

        Assert.assertNotEquals( Value.of( "" ), Value.of( " " ) );
        Assert.assertNotEquals( Value.of( "1" ), Value.of( "2" ) );
        Assert.assertNotEquals( Value.of( "" ), Value.of( (Object) null ) );
        Assert.assertNotEquals( Value.of( "" ), Value.of( 123 ) );
        Assert.assertNotEquals( Value.of( "" ), Value.of( false ) );

    }

    /**
     * Test the equals on numbers.
     */
    @Test
    public void testNumberEquals() throws Exception {

        Assert.assertEquals( Value.of( 123 ), Value.of( (byte) 123 ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( (short) 123 ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( 123 ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( (long) 123 ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( 123f ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( 123d ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( new BigDecimal( "123" ) ) );
        Assert.assertEquals( Value.of( 123 ), Value.of( new BigInteger( "123" ) ) );
        Assert.assertEquals( Value.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ),
                             Value.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ) );
        Assert.assertEquals( Value.of( new BigInteger(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ),
                             Value.of( new BigDecimal(
                                     "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123.00" ) ) );
        Assert.assertEquals( Value.of( new BigDecimal( "0.0" ) ), Value.of( new BigDecimal( "0" ) ) );

        Assert.assertNotEquals( Value.of( 123 ), Value.of( (Object) null ) );
        Assert.assertNotEquals( Value.of( 123 ), Value.of( true ) );
        Assert.assertNotEquals( Value.of( 123 ), Value.of( "123" ) );
        Assert.assertNotEquals( Value.of( 123 ),
                                Value.of( new BigInteger(
                                        "123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123123" ) ) );
        Assert.assertNotEquals( Value.of( 123 ), Value.of( 123.1 ) );

    }

    @Test
    public void testMapEquals() throws Exception {
        ValueMap map1 = ValueMap.create();
        map1.put( "null", Value.of( (Object) null ) );
        map1.put( "int", Value.of( 123 ) );

        ValueMap map2 = ValueMap.create();
        map2.put( "int", Value.of( 123 ) );
        map2.put( "null", Value.of( (Object) null ) );

        ValueMap map3 = ValueMap.create();
        map3.put( "null", Value.of( (Object) null ) );
        map3.put( "int", Value.of( "123" ) );

        Assert.assertEquals( map1, map2 );
        Assert.assertNotEquals( map1, map3 );
        Assert.assertNotEquals( map2, map3 );

    }

    @Test
    public void testListEquals() throws Exception {

        ValueList list1 = ValueList.create();
        list1.add( Value.of( (Object) null ) );
        list1.add( Value.of( 123 ) );

        ValueList list2 = ValueList.create();
        list2.add( Value.of( (Object) null ) );
        list2.add( Value.of( 123 ) );

        ValueList list3 = ValueList.create();
        list3.add( Value.of( 123 ) );
        list3.add( Value.of( (Object) null ) );

        Assert.assertEquals( list1, list2 );
        Assert.assertNotEquals( list1, list3 );
        Assert.assertNotEquals( list2, list3 );

    }
}
