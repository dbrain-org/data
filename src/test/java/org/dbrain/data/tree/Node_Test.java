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
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test Value methods that are not tested otherwise.
 */
public class Node_Test {

    /**
     * Test the isNull method.
     */
    @Test
    public void testIsNull() throws Exception {

        Node v;

        v = Node.of( "" );
        Assert.assertFalse( v.isNull() );

        v = Node.of( 0.0D );
        Assert.assertFalse( v.isNull() );

        v = Node.of( Boolean.TRUE );
        Assert.assertFalse( v.isNull() );

        v = NodeList.newInstance();
        Assert.assertFalse( v.isNull() );

        v = NodeMap.newInstance();
        Assert.assertFalse( v.isNull() );

        v = Node.of( (Boolean) null );
        Assert.assertTrue( v.isNull() );

        v = Node.of( (Double) null );
        Assert.assertTrue( v.isNull() );

        v = Node.of( (String) null );
        Assert.assertTrue( v.isNull() );

        v = Node.nullValue();
        Assert.assertTrue( v.isNull() );

    }

    @Test
    public void testHashCode() throws Exception {
        Node v1 = Node.of( 2 );
        Node v2 = Node.of( 2 );
        Assert.assertTrue( v1.hashCode() == v2.hashCode() );

        v1 = Node.of( (Object) null );
        v2 = Node.of( (Object) null );
        Assert.assertTrue( v1.hashCode() == v2.hashCode() );

    }
}
