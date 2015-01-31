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
 * Test toFloat.
 */
public class Casts_toFloat_Test {

    @Test
    public void testFromString() throws Exception {
        Float d1 = Casts.toFloat( "10.1" );
        Float d2 = Casts.toFloat( "" );
        Float d3 = Casts.toFloat( "" );

        Assert.assertEquals( new Float( 10.1f ), d1 );
        Assert.assertNull( d2 );
        Assert.assertNull( d3 );

    }

}
