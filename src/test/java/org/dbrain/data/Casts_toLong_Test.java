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

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;

/**
 * Created by epoitras on 19/01/15.
 */
public class Casts_toLong_Test {

    @Test
    public void testFromString() throws Exception {
        Long v1 = Casts.toLong( "10" );
        Long v2 = Casts.toLong( "" );
        Long v3 = Casts.toLong( " " );

        Assert.assertEquals( new Long( 10 ), v1 );
        Assert.assertNull( v2 );
        Assert.assertNull( v3 );
    }

    @Test
    public void testFromBigDecimal() throws Exception {
        Long l1 = Casts.toLong( (BigDecimal) null );
        Long l2 = Casts.toLong( new BigDecimal( "10" ) );

        Assert.assertNull( l1 );
        Assert.assertEquals( new Long( 10l ), l2 );

    }

    @Test( expected = ArithmeticException.class )
    public void testFromBigDecimalFail() throws Exception {
        Casts.toLong( new BigDecimal( "100000000000000000000000000000000000000000000000000000000" ));
    }
}
