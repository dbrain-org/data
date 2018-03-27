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

import org.dbrain.data.tree.Node;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;

/**
 * Test different valueOf.
 */
public class Node_of_Test {


    @Test
    public void testOfObject() throws Exception {

        Assert.assertTrue( Node.of( (Object) null ).isNull() );
        Assert.assertEquals( Node.of( (Object) "" ), Node.of( "" ) );
        Assert.assertEquals( Node.of( (Object) Node.of( "" ) ), Node.of( "" ) );
        Assert.assertEquals( Node.of( (Object) new Byte( (byte) 1 ) ), Node.of( new Byte( (byte) 1 ) ) );
        Assert.assertEquals( Node.of( (Object) new Short( (byte) 1 ) ), Node.of( new Short( (byte) 1 ) ) );
        Assert.assertEquals( Node.of( (Object) new Integer( (byte) 1 ) ), Node.of( new Integer( (byte) 1 ) ) );
        Assert.assertEquals( Node.of( (Object) new Long( (byte) 1 ) ), Node.of( new Long( (byte) 1 ) ) );
        Assert.assertEquals( Node.of( (Object) 1.0f ), Node.of( 1.0f ) );
        Assert.assertEquals( Node.of( (Object) 1.0D ), Node.of( 1.0D ) );
        Assert.assertEquals( Node.of( (Object) Boolean.FALSE ), Node.of( false ) );

        // TODO Test of generic object serialization.
    }

    @Test
    public void testOfByte() throws Exception {
        Assert.assertTrue( Node.of( (Byte) null ).isNull() );
        Assert.assertEquals( Node.of( new Byte( (byte) 0 ) ).getObject(), new BigDecimal( (byte) 0 ) );
    }

    @Test
    public void testOfShort() throws Exception {
        Assert.assertTrue( Node.of( (Short) null ).isNull() );
        Assert.assertEquals( Node.of( new Short( (byte) 0 ) ).getObject(), new BigDecimal( (byte) 0 ) );
    }

    @Test
    public void testOfInteger() throws Exception {
        Assert.assertTrue( Node.of( (Integer) null ).isNull() );
        Assert.assertEquals( Node.of( new Integer( (byte) 0 ) ).getObject(), new BigDecimal( (byte) 0 ) );
    }

    @Test
    public void testOfLong() throws Exception {
        Assert.assertTrue( Node.of( (Long) null ).isNull() );
        Assert.assertEquals( Node.of( new Long( (byte) 0 ) ).getObject(), new BigDecimal( (byte) 0 ) );
    }

    @Test
    public void testOfFloat() throws Exception {
        Assert.assertTrue( Node.of( (Float) null ).isNull() );
        Assert.assertEquals( Node.of( new Float( (byte) 0 ) ).getDouble().doubleValue(), 0d, 0.0000000000001 );

        Float f = 10f / 3;
        Assert.assertEquals( Node.of( f ).getFloat(), f, .0000000000001f );

        Assert.assertNull( Node.of( Float.NaN ).getObject() );

    }

    @Test
    public void testOfDouble() throws Exception {
        Assert.assertTrue( Node.of( (Double) null ).isNull() );
        Assert.assertEquals( Node.of( new Double( (byte) 0 ) ).getObject(), new BigDecimal( (byte) 0 ) );

        Assert.assertEquals( Node.of( Double.NaN ), Node.nullValue() );

    }

    @Test
    public void testOfBoolean() throws Exception {
        Assert.assertTrue( Node.of( (Boolean) null ).isNull() );
        Assert.assertEquals( Node.of( Boolean.FALSE ).getObject(), Boolean.FALSE );
    }

    @Test
    public void testOfString() throws Exception {
        Assert.assertTrue( Node.of( (String) null ).isNull() );
        Assert.assertEquals( Node.of( "Test" ).getObject(), "Test" );
    }

    @Test
    public void testOfCharSequence() throws Exception {
        Assert.assertTrue( Node.of( (CharSequence) null ).isNull() );
        StringBuilder sb = new StringBuilder();
        sb.append( "Test" );
        Assert.assertEquals( Node.of( sb ).getObject(), "Test" );
    }

}
