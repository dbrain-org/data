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

/**
 * Test different valueOf.
 */
public class Value_of_Test {


    @Test
    public void testOfObject() throws Exception {

        Assert.assertTrue( Value.of( (Object) null ).isNull() );
        Assert.assertEquals( Value.of( (Object) "" ), Value.of( "" ) );
        Assert.assertEquals( Value.of( (Object) Value.of( "" ) ), Value.of( "" ) );
        Assert.assertEquals( Value.of( (Object) new Byte( (byte)1 )), Value.of( new Byte( (byte)1 ) ) );
        Assert.assertEquals( Value.of( (Object) new Short( (byte)1 )), Value.of( new Short( (byte)1 ) ) );
        Assert.assertEquals( Value.of( (Object) new Integer( (byte)1 )), Value.of( new Integer( (byte)1 ) ) );
        Assert.assertEquals( Value.of( (Object) new Long( (byte)1 )), Value.of( new Long( (byte)1 ) ) );
        Assert.assertEquals( Value.of( (Object) 1.0f ), Value.of( 1.0f ) );
        Assert.assertEquals( Value.of( (Object) 1.0D ), Value.of( 1.0D ) );
        Assert.assertEquals( Value.of( (Object) Boolean.FALSE ), Value.of( false ) );

        // TODO Test of generic object serialization.
    }

    @Test
    public void testOfByte() throws Exception {
        Assert.assertTrue( Value.of( (Byte) null ).isNull() );
        Assert.assertEquals( Value.of( new Byte( (byte) 0 ) ).getObject(), new Byte( (byte) 0 ) );
    }

    @Test
    public void testOfShort() throws Exception {
        Assert.assertTrue( Value.of( (Short) null ).isNull() );
        Assert.assertEquals( Value.of( new Short( (byte) 0 ) ).getObject(), new Short( (byte) 0 ) );
    }

    @Test
    public void testOfInteger() throws Exception {
        Assert.assertTrue( Value.of( (Integer) null ).isNull() );
        Assert.assertEquals( Value.of( new Integer( (byte) 0 ) ).getObject(), new Integer( (byte) 0 ) );
    }

    @Test
    public void testOfLong() throws Exception {
        Assert.assertTrue( Value.of( (Long) null ).isNull() );
        Assert.assertEquals( Value.of( new Long( (byte) 0 ) ).getObject(), new Long( (byte) 0 ) );
    }

    @Test
    public void testOfFloat() throws Exception {
        Assert.assertTrue( Value.of( (Float) null ).isNull() );
        Assert.assertEquals( Value.of( new Float( (byte) 0 ) ).getObject(), new Float( (byte) 0 ) );
    }

    @Test
    public void testOfDouble() throws Exception {
        Assert.assertTrue( Value.of( (Double) null ).isNull() );
        Assert.assertEquals( Value.of( new Double( (byte) 0 ) ).getObject(), new Double( (byte) 0 ) );
    }

    @Test
    public void testOfBoolean() throws Exception {
        Assert.assertTrue( Value.of( (Boolean) null ).isNull() );
        Assert.assertEquals( Value.of( Boolean.FALSE ).getObject(), Boolean.FALSE );
    }

    @Test
    public void testOfString() throws Exception {
        Assert.assertTrue( Value.of( (String) null ).isNull() );
        Assert.assertEquals( Value.of( "Test" ).getObject(), "Test" );
    }


}