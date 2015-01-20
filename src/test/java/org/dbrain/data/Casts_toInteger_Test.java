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
public class Casts_toInteger_Test {

    @Test
    public void testFromString() throws Exception {
        Integer v1 = Casts.toInteger( "10" );
        Integer v2 = Casts.toInteger( "" );
        Integer v3 = Casts.toInteger( " " );

        Assert.assertEquals( new Integer( 10 ), v1 );
        Assert.assertNull( v2 );
        Assert.assertNull( v3 );

    }

    @Test
    public void testFromBigDecimal() throws Exception {
        Integer v1 = Casts.toInteger( (BigDecimal) null );
        Integer v2 = Casts.toInteger( new BigDecimal( "10" ) );

        Assert.assertNull( v1 );
        Assert.assertEquals( new Integer( 10 ), v2 );

    }

    @Test( expected = DataCoercionException.class )
    public void testFromBigDecimalFail() throws Exception {
        Casts.toInteger( new BigDecimal( "100000000000000000000000000000000000000000000000000000000" ) );
    }
}
