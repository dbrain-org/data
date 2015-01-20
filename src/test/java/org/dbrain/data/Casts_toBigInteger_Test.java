/*
 * Copyright [2015] [Eric Poitras]
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

import java.math.BigInteger;

/**
 * Test toBigInteger.
 */
public class Casts_toBigInteger_Test {

    @Test
    public void testFromString() throws Exception {
        BigInteger d1 = Casts.toBigInteger( "10" );
        BigInteger d2 = Casts.toBigInteger( "" );
        BigInteger d3 = Casts.toBigInteger( " " );

        Assert.assertEquals( new BigInteger( "10" ), d1 );
        Assert.assertNull( d2 );
        Assert.assertNull( d3 );

    }

    @Test( expected = DataCoercionException.class )
    public void testFromObject() throws Exception {
        Casts.toBigInteger( new Object() );
    }
}
