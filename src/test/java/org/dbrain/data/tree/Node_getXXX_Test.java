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

import junit.framework.AssertionFailedError;
import org.dbrain.data.DataCoercionException;
import org.dbrain.data.tree.Node;
import org.junit.Assert;
import org.junit.Test;

/**
 * Fun with getXXX methods.
 */
public class Node_getXXX_Test {

    @Test
    public void test_getFromBoolean() throws Exception {

        Node v = Node.of( true );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( Boolean.TRUE.toString(), v.getString() );

        v = Node.of( false );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( Boolean.FALSE.toString(), v.getString() );

    }

    @Test
    public void test_getFromBoolean_fail() throws Exception {

        Node v = Node.of( true );
        try {
            v.getByte();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }

        try {
            v.getShort();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }

        try {
            v.getInt();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }

        try {
            v.getLong();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }

        try {
            v.getFloat();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }

        try {
            v.getDouble();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( DataCoercionException e ) {
            // Expected
        }
    }

    @Test
    public void test_getFromDouble() throws Exception {

        Node v = Node.of( 123D );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 123 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 123 ), v.getShort() );
        Assert.assertEquals( new Integer( 123 ), v.getInt() );
        Assert.assertEquals( new Long( 123 ), v.getLong() );
        Assert.assertEquals( "123", v.getString() );
        Assert.assertEquals( new Float( 123.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 123.0 ), v.getDouble() );

        v = Node.of( 0D );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 0 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0", v.getString() );
        Assert.assertEquals( new Float( 0.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 0.0 ), v.getDouble() );

        v = Node.of( (Double) null );
        Assert.assertNull( v.getObject() );
        Assert.assertTrue( v.isNull() );

    }

    @Test
    public void test_getFromDouble_fail() throws Exception {

        Node v = Node.of( 123.1D );
        try {
            v.getByte();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( ArithmeticException e ) {
            // Expected
        }

        try {
            v.getShort();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( ArithmeticException e ) {
            // Expected
        }

        try {
            v.getInt();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( ArithmeticException e ) {
            // Expected
        }

        try {
            v.getLong();
            throw new AssertionFailedError( "Should not happend." );
        } catch ( ArithmeticException e ) {
            // Expected
        }
    }


    @Test
    public void test_getFromString() throws Exception {
        Node v = Node.of( "123" );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 123 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 123 ), v.getShort() );
        Assert.assertEquals( new Integer( 123 ), v.getInt() );
        Assert.assertEquals( new Long( 123 ), v.getLong() );
        Assert.assertEquals( "123", v.getString() );
        Assert.assertEquals( new Float( 123.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 123.0 ), v.getDouble() );

        v = Node.of( "0" );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 0 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0", v.getString() );
        Assert.assertEquals( new Float( 0.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 0.0 ), v.getDouble() );

        v = Node.of( "1.0" );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 1 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 1 ), v.getShort() );
        Assert.assertEquals( new Integer( 1 ), v.getInt() );
        Assert.assertEquals( new Long( 1 ), v.getLong() );
        Assert.assertEquals( "1.0", v.getString() );
        Assert.assertEquals( new Float( 1.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 1.0 ), v.getDouble() );

        v = Node.of( "0.0" );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte) 0 ), v.getByte() );
        Assert.assertEquals( new Short( (short) 0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0.0", v.getString() );
        Assert.assertEquals( new Float( 0.0f ), v.getFloat() );
        Assert.assertEquals( new Double( 0.0 ), v.getDouble() );

    }

}
