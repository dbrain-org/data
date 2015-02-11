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

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Value_List_Test {

    @Test
    public void testNewInstance() throws Exception {
        ValueList list;

        // Simple constructor
        list = ValueList.newInstance();
        Assert.assertEquals( list.size(), 0 );
    }

    @Test
    public void testNewBuilder() throws Exception {
        ValueList vl = ValueList.newBuilder() //
                .addNull() //
                .add( new Byte( (byte) 1 ) ) //
                .add( new Short( (short) 2 ) ) //
                .add( new Integer( 3 ) ) //
                .add( new Long( 4l ) ) //
                .add( new BigInteger( "5" ) ) //
                .add( new BigDecimal( "6.1" ) ) //
                .add( "string" ) //
                .add( true ) //
                .add( 1.1f ) //
                .add( 1.2d ) //
                .add( Value.of( "value" ) ) //
                .build();

        Assert.assertTrue( vl.get( 0 ).equals( Value.nullValue() ) );
        Assert.assertTrue( vl.get( 1 ).equals( Value.of( 1 ) ) );
        Assert.assertTrue( vl.get( 2 ).equals( Value.of( 2 ) ) );
        Assert.assertTrue( vl.get( 3 ).equals( Value.of( 3 ) ) );
        Assert.assertTrue( vl.get( 4 ).equals( Value.of( 4 ) ) );
        Assert.assertTrue( vl.get( 5 ).equals( Value.of( 5 ) ) );
        Assert.assertTrue( vl.get( 6 ).equals( Value.of( 6.1 ) ) );
        Assert.assertTrue( vl.get( 7 ).equals( Value.of( "string" ) ) );
        Assert.assertTrue( vl.get( 8 ).equals( Value.of( true ) ) );
        Assert.assertTrue( vl.get( 9 ).equals( Value.of( 1.1f ) ) );
        Assert.assertTrue( vl.get( 10 ).equals( Value.of( 1.2d ) ) );
        Assert.assertTrue( vl.get( 11 ).equals( Value.of( "value" ) ) );


    }

    @Test( expected = DataCoercionException.class )
    public void testAsString() throws Exception {
        ValueList.newInstance().getString();
    }
}
