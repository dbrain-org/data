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
 * Created by epoitras on 30/06/14.
 */
public class Value_getXXX_Test {

    @Test
    public void test_getFromBoolean() throws Exception {

        Value v = Value.of( true );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
//        Assert.assertEquals( new Byte( (byte)123 ), v.getByte() );
//        Assert.assertEquals( new Short( (short)123 ), v.getShort() );
//        Assert.assertEquals( new Integer( 123 ), v.getInt() );
//        Assert.assertEquals( new Long( 123 ), v.getLong() );
        Assert.assertEquals( Boolean.TRUE.toString(), v.getString() );
//        Assert.assertEquals( 123.0f, v.getFloat() );
//        Assert.assertEquals( 123.0, v.getDouble() );

        v = Value.of( false );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
//        Assert.assertEquals( new Byte( (byte)0 ), v.getByte() );
//        Assert.assertEquals( new Short( (short)0 ), v.getShort() );
//        Assert.assertEquals( new Integer( 0 ), v.getInt() );
//        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( Boolean.FALSE.toString(), v.getString() );
//        Assert.assertEquals( 0.0f, v.getFloat() );
//        Assert.assertEquals( 0.0, v.getDouble() );
    }


    @Test
    public void test_getFromDouble() throws Exception {

        Value v = Value.of( 123D );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)123 ), v.getByte() );
        Assert.assertEquals( new Short( (short)123 ), v.getShort() );
        Assert.assertEquals( new Integer( 123 ), v.getInt() );
        Assert.assertEquals( new Long( 123 ), v.getLong() );
        Assert.assertEquals( "123.0", v.getString() );
        Assert.assertEquals( 123.0f, v.getFloat() );
        Assert.assertEquals( 123.0, v.getDouble() );

        v = Value.of( 0D );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)0 ), v.getByte() );
        Assert.assertEquals( new Short( (short)0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0.0", v.getString() );
        Assert.assertEquals( 0.0f, v.getFloat() );
        Assert.assertEquals( 0.0, v.getDouble() );
    }



    @Test
    public void test_getFromString() throws Exception {
        Value v = Value.of( "123" );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)123 ), v.getByte() );
        Assert.assertEquals( new Short( (short)123 ), v.getShort() );
        Assert.assertEquals( new Integer( 123 ), v.getInt() );
        Assert.assertEquals( new Long( 123 ), v.getLong() );
        Assert.assertEquals( "123", v.getString() );
        Assert.assertEquals( 123.0f, v.getFloat() );
        Assert.assertEquals( 123.0, v.getDouble() );

        v = Value.of( "0" );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)0 ), v.getByte() );
        Assert.assertEquals( new Short( (short)0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0", v.getString() );
        Assert.assertEquals( 0.0f, v.getFloat() );
        Assert.assertEquals( 0.0, v.getDouble() );

        v = Value.of( "1.0" );
        Assert.assertEquals( Boolean.TRUE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)1 ), v.getByte() );
        Assert.assertEquals( new Short( (short)1 ), v.getShort() );
        Assert.assertEquals( new Integer( 1 ), v.getInt() );
        Assert.assertEquals( new Long( 1 ), v.getLong() );
        Assert.assertEquals( "1.0", v.getString() );
        Assert.assertEquals( 1.0f, v.getFloat() );
        Assert.assertEquals( 1.0, v.getDouble() );

        v = Value.of( "0.0" );
        Assert.assertEquals( Boolean.FALSE, v.getBoolean() );
        Assert.assertEquals( new Byte( (byte)0 ), v.getByte() );
        Assert.assertEquals( new Short( (short)0 ), v.getShort() );
        Assert.assertEquals( new Integer( 0 ), v.getInt() );
        Assert.assertEquals( new Long( 0 ), v.getLong() );
        Assert.assertEquals( "0.0", v.getString() );
        Assert.assertEquals( 0.0f, v.getFloat() );
        Assert.assertEquals( 0.0, v.getDouble() );

    }

}
