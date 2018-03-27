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
import org.dbrain.data.tree.NodeMap;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class NodeMap_Test {

    @Test
    public void testConstruction() throws Exception {
        NodeMap map;

        // Simple constructor
        map = NodeMap.newInstance();
        assertEquals( map.size(), 0 );
    }

    @Test( expected = DataCoercionException.class )
    public void testGetBoolean() throws Exception {
        NodeMap.newInstance().getBoolean();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetByte() throws Exception {
        NodeMap.newInstance().getByte();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetShort() throws Exception {
        NodeMap.newInstance().getShort();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetInteger() throws Exception {
        NodeMap.newInstance().getInt();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetLong() throws Exception {
        NodeMap.newInstance().getLong();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetFloat() throws Exception {
        NodeMap.newInstance().getFloat();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetDouble() throws Exception {
        NodeMap.newInstance().getDouble();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetString() throws Exception {
        NodeMap.newInstance().getString();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetList() throws Exception {
        NodeMap.newInstance().getList();
    }

    @Test
    public void testGetObject() throws Exception {
        NodeMap m = NodeMap.newInstance();
        m.put( "key", Node.of( "value" ) );

        Object o = m.getObject();
        assertTrue( o instanceof java.util.Map );
        java.util.Map m2 = (java.util.Map) o;
        assertEquals( 1, m2.size() );
        assertTrue( m2.containsKey( "key" ) );
        assertEquals( m2.get( "key" ), "value" );

    }

    @Test
    public void testIsEmpty() throws Exception {
        NodeMap m = NodeMap.newInstance();

        assertTrue( m.isEmpty() );
        m.put( "test", Node.of( "Test" ) );
        Assert.assertFalse( m.isEmpty() );
        m.remove( "test" );
        assertTrue( m.isEmpty() );

    }

    @Test
    public void testPutAll() throws Exception {
        NodeMap m1 = NodeMap.newInstance();
        NodeMap m2 = NodeMap.newInstance();

        m1.put( "test1", Node.of( "Test" ) );
        m1.put( "test2", Node.of( "Test2" ) );
        m1.put( "test3", Node.of( "Test3" ) );

        m2.putAll( m1 );
        assertEquals( m1, m2 );
    }

    @Test
    public void testClear() throws Exception {
        NodeMap m1 = NodeMap.newInstance();
        m1.put( "test1", Node.of( "Test" ) );
        m1.put( "test2", Node.of( "Test2" ) );
        m1.put( "test3", Node.of( "Test3" ) );

        m1.clear();
        assertTrue( m1.isEmpty() );
        assertEquals( 0, m1.size() );


    }

    @Test
    public void testGetValue() {
        String k = "test";
        Node s = Node.of( "String" );

        NodeMap v = NodeMap.newInstance();

        Assert.assertFalse( v.containsKey( k ) );
        Assert.assertFalse( v.containsValue( s ) );
        assertEquals( 0, v.size() );

        v.put( k, Node.of( "String" ) );
        assertTrue( v.containsKey( k ) );
        assertTrue( v.containsValue( s ) );
        assertEquals( 1, v.size() );
        assertEquals( s, v.get( "test" ) );

        assertEquals( s, v.remove( "test" ) );
        Assert.assertFalse( v.containsKey( k ) );
        Assert.assertFalse( v.containsValue( s ) );
        assertEquals( 0, v.size() );

        assertTrue( v.get( "non-existing" ).isNull() );
    }

    @Test
    public void testNewBuilder() throws Exception {
        NodeMap vl = NodeMap.newBuilder() //
                .putNull( "0" ) //
                .put( "1", new Byte( (byte) 1 ) ) //
                .put( "2", new Short( (short) 2 ) ) //
                .put( "3", new Integer( 3 ) ) //
                .put( "4", new Long( 4l ) ) //
                .put( "5", new BigInteger( "5" ) ) //
                .put( "6", new BigDecimal( "6.1" ) ) //
                .put( "7", "string" ) //
                .put( "8", true ) //
                .put( "9", 1.1f ) //
                .put( "10", 1.2d ) //
                .put( "11", Node.of( "value" ) ) //
                .build();

        Assert.assertTrue( vl.get( "0" ).equals( Node.nullValue() ) );
        Assert.assertTrue( vl.get( "1" ).equals( Node.of( 1 ) ) );
        Assert.assertTrue( vl.get( "2" ).equals( Node.of( 2 ) ) );
        Assert.assertTrue( vl.get( "3" ).equals( Node.of( 3 ) ) );
        Assert.assertTrue( vl.get( "4" ).equals( Node.of( 4 ) ) );
        Assert.assertTrue( vl.get( "5" ).equals( Node.of( 5 ) ) );
        Assert.assertTrue( vl.get( "6" ).equals( Node.of( 6.1 ) ) );
        Assert.assertTrue( vl.get( "7" ).equals( Node.of( "string" ) ) );
        Assert.assertTrue( vl.get( "8" ).equals( Node.of( true ) ) );
        Assert.assertTrue( vl.get( "9" ).equals( Node.of( 1.1f ) ) );
        Assert.assertTrue( vl.get( "10" ).equals( Node.of( 1.2d ) ) );
        Assert.assertTrue( vl.get( "11" ).equals( Node.of( "value" ) ) );

    }
}