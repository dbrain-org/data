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

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by epoitras on 03/10/14.
 */
public class Fqn_toString_Test {

    @Test
    public void testToString() throws Exception {
        Fqn d;

        d = Fqn.of( "test" );
        Assert.assertEquals( "test", d.toString() );

        d = Fqn.of( "test.''");
        Assert.assertEquals( "test.''", d.toString() );

        d = Fqn.of( "''" );
        Assert.assertEquals( "''", d.toString() );

        d = Fqn.of( "''.test.'test*'.'test'''.test.''" );
        Assert.assertEquals( "''.test.'test*'.'test'''.test.''", d.toString() );

    }
}
