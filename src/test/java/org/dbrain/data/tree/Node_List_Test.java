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

package org.dbrain.data.tree;

import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

public class Node_List_Test {

    @Test
    public void testNewInstance() throws Exception {
        NodeList list;

        // Simple constructor
        list = NodeList.newInstance();
        Assert.assertEquals( list.size(), 0 );
    }

    @Test
    public void testNewBuilder() throws Exception {
        NodeList vl = NodeList.newBuilder() //
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
                .add( Node.of( "value" ) ) //
                .build();

        Assert.assertTrue( vl.get( 0 ).equals( Node.nullValue() ) );
        Assert.assertTrue( vl.get( 1 ).equals( Node.of( 1 ) ) );
        Assert.assertTrue( vl.get( 2 ).equals( Node.of( 2 ) ) );
        Assert.assertTrue( vl.get( 3 ).equals( Node.of( 3 ) ) );
        Assert.assertTrue( vl.get( 4 ).equals( Node.of( 4 ) ) );
        Assert.assertTrue( vl.get( 5 ).equals( Node.of( 5 ) ) );
        Assert.assertTrue( vl.get( 6 ).equals( Node.of( 6.1 ) ) );
        Assert.assertTrue( vl.get( 7 ).equals( Node.of( "string" ) ) );
        Assert.assertTrue( vl.get( 8 ).equals( Node.of( true ) ) );
        Assert.assertTrue( vl.get( 9 ).equals( Node.of( 1.1f ) ) );
        Assert.assertTrue( vl.get( 10 ).equals( Node.of( 1.2d ) ) );
        Assert.assertTrue( vl.get( 11 ).equals( Node.of( "value" ) ) );

    }

    @Test
    public void testOf() throws Exception {
        NodeList v1 = NodeList.newBuilder() //
                 .add( 12 )             //
                 .add( 23 )              //
                 .add( 34 )               //
                 .build();
        NodeList v2 = NodeList.of( new Object[]{ 12, 23, 34 } );
        NodeList v3 = NodeList.of( Arrays.asList( 12, 23, 34 ) );
        NodeList v4 = NodeList.asList( 12, 23, 34 );
        Assert.assertEquals( v1, v2 );
        Assert.assertEquals( v1, v3 );
        Assert.assertEquals( v1, v4 );

        Assert.assertNull( NodeList.of( (Object[]) null ));
        Assert.assertNull( NodeList.of( (Iterable) null ));

    }

    @Test( expected = DataCoercionException.class )
    public void testAsString() throws Exception {
        NodeList.newInstance().getString();
    }
}
