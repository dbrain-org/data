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
 * Created by epoitras on 19/01/15.
 */
public class Casts_toBoolean_Test {

    @Test
    public void testFromString() throws Exception {

        Boolean v1 = Casts.toBoolean( "true" );
        Boolean v2 = Casts.toBoolean( "false" );
        Boolean v3 = Casts.toBoolean( " " );
        Boolean v4 = Casts.toBoolean( "" );

        Assert.assertTrue( v1 );
        Assert.assertFalse( v2 );
        Assert.assertNull( v3 );
        Assert.assertNull( v4 );

    }
}
