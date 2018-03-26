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

package org.dbrain.data.casts;

import org.dbrain.data.cast.Enums;
import org.junit.Assert;
import org.junit.Test;

import java.math.RoundingMode;

/**
 * Created by epoitras on 19/01/15.
 */
public class Casts_toEnum_Test {

    @Test
    public void testFromName() throws Exception {
        RoundingMode rm1 = Enums.toEnum( RoundingMode.class, RoundingMode.CEILING.name() );
        RoundingMode rm2 = Enums.toEnum( RoundingMode.class, " " + RoundingMode.CEILING.name() + " " );
        RoundingMode rm3 = Enums.toEnum( RoundingMode.class, "" );
        RoundingMode rm4 = Enums.toEnum( RoundingMode.class, " " );
        RoundingMode rm5 = Enums.toEnum( RoundingMode.class, null );

        Assert.assertEquals( RoundingMode.CEILING, rm1 );
        Assert.assertEquals( RoundingMode.CEILING, rm2 );
        Assert.assertNull( rm3 );
        Assert.assertNull( rm4 );
        Assert.assertNull( rm5 );
    }

    @Test
    public void testToName() throws Exception {
        String v1 = Enums.toEnumName( RoundingMode.CEILING );
        String v2 = Enums.toEnumName( null );

        Assert.assertEquals( RoundingMode.CEILING.name(), v1 );
        Assert.assertNull( v2 );
    }
}
