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
    public void testGetString() throws Exception {
        Value.newMap().getString();
    }

    @Test
    public void testGetValue() {
        String k = "test";
        Value s = Value.of("String");

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
