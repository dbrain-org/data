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

import junit.framework.Assert;
import org.junit.Test;

public class Value_Map_Test {

    @Test
    public void testConstruction() throws Exception {
        Value.Map map;

        // Simple constructor
        map = Value.newMap();
        Assert.assertEquals( map.size(), 0 );
    }

    @Test( expected = DataCoercionException.class )
    public void testGetBoolean() throws Exception {
        Value.newMap().getBoolean();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetByte() throws Exception {
        Value.newMap().getByte();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetShort() throws Exception {
        Value.newMap().getShort();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetInteger() throws Exception {
        Value.newMap().getInt();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetLong() throws Exception {
        Value.newMap().getLong();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetFloat() throws Exception {
        Value.newMap().getFloat();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetDouble() throws Exception {
        Value.newMap().getDouble();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetString() throws Exception {
        Value.newMap().getString();
    }

    @Test( expected = DataCoercionException.class )
    public void testGetList() throws Exception {
        Value.newMap().getList();
    }

    @Test
    public void testGetObject() throws Exception {
        Value.Map m = Value.newMap();
        m.put( "key", Value.of( "value" ) );

        Object o = m.getObject();
        Assert.assertTrue( o instanceof java.util.Map );
        java.util.Map m2 = (java.util.Map) o;
        Assert.assertEquals( 1, m2.size() );
        Assert.assertTrue( m2.containsKey( "key" ) );
        Assert.assertEquals( m2.get( "key" ), "value" );

    }

    @Test
    public void testIsEmpty() throws Exception {
        Value.Map m = Value.newMap();

        Assert.assertTrue( m.isEmpty() );
        m.put( "test", Value.of( "Test" ) );
        Assert.assertFalse( m.isEmpty() );
        m.remove( "test" );
        Assert.assertTrue( m.isEmpty() );

    }

    @Test
    public void testPutAll() throws Exception {
        Value.Map m1 = Value.newMap();
        Value.Map m2 = Value.newMap();

        m1.put( "test1", Value.of( "Test" ) );
        m1.put( "test2", Value.of( "Test2" ) );
        m1.put( "test3", Value.of( "Test3" ) );

        m2.putAll( m1 );
        Assert.assertEquals( m1, m2 );
    }

    @Test
    public void testClear() throws Exception {
        Value.Map m1 = Value.newMap();
        m1.put( "test1", Value.of( "Test" ) );
        m1.put( "test2", Value.of( "Test2" ) );
        m1.put( "test3", Value.of( "Test3" ) );

        m1.clear();
        Assert.assertTrue( m1.isEmpty() );
        Assert.assertEquals( 0, m1.size() );


    }

    @Test
    public void testGetValue() {
        String k = "test";
        Value s = Value.of( "String" );

        Value.Map v = Value.newMap();

        Assert.assertFalse( v.containsKey( k ) );
        Assert.assertFalse( v.containsValue( s ) );
        Assert.assertEquals( 0, v.size() );

        v.put( k, Value.of( "String" ) );
        Assert.assertTrue( v.containsKey( k ) );
        Assert.assertTrue( v.containsValue( s ) );
        Assert.assertEquals( 1, v.size() );
        Assert.assertEquals( s, v.get( "test" ) );

        Assert.assertEquals( s, v.remove( "test" ) );
        Assert.assertFalse( v.containsKey( k ) );
        Assert.assertFalse( v.containsValue( s ) );
        Assert.assertEquals( 0, v.size() );

        Assert.assertTrue( v.get( "non-existing" ).isNull() );
    }

}
