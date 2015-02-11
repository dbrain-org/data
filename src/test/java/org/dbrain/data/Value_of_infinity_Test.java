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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

/**
 * Test different valueOf.
 */
@RunWith( Parameterized.class )
public class Value_of_infinity_Test {

    private Object v;

    public Value_of_infinity_Test( Object v ) {
        this.v = v;
    }

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              new Object[]{ Double.NEGATIVE_INFINITY }, //
                              new Object[]{ Double.POSITIVE_INFINITY }, //
                              new Object[]{ Float.NEGATIVE_INFINITY }, //
                              new Object[]{ Float.POSITIVE_INFINITY } //
                            );
    }

    @Test( expected = DataCoercionException.class )
    public void testInvalidNumbers() throws Exception {
        Value.of( v );
    }

}
