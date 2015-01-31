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

/**
 * Test toDouble.
 */
public class Casts_toDouble_Test {

    @Test
    public void testFromString() throws Exception {
        Double d1 = Casts.toDouble( "10.1" );
        Double d2 = Casts.toDouble( "" );
        Double d3 = Casts.toDouble( " " );

        Assert.assertEquals( new Double( 10.1d ), d1 );
        Assert.assertNull( d2 );
        Assert.assertNull( d3 );

    }
}
