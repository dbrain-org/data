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

package org.dbrain.data.cast;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;

/**
 * Test a large range of casting that fails.
 */
@RunWith( Parameterized.class )
public class Casts_toXXXX_Fail_Test {

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              // Boolean
                              new Object[]{ (Function<Object, Object>) Booleans::toBoolean, "invalid" },
                              new Object[]{ (Function<Object, Object>) Booleans::toBoolean, new Object() },

                              new Object[]{ (Function<Object, Object>) Bytes::toByte, "invalid" },
                              new Object[]{ (Function<Object, Object>) Bytes::toByte, new Object() },
                              new Object[]{ (Function<Object, Object>) Shorts::toShort, "invalid" },
                              new Object[]{ (Function<Object, Object>) Shorts::toShort, new Object() },
                              new Object[]{ (Function<Object, Object>) Integers::toInteger, "invalid" },
                              new Object[]{ (Function<Object, Object>) Integers::toInteger, new Object() },
                              new Object[]{ (Function<Object, Object>) Longs::toLong, "invalid" },
                              new Object[]{ (Function<Object, Object>) Longs::toLong, new Object() },
                              new Object[]{ (Function<Object, Object>) BigIntegers::toBigInteger, "invalid" },
                              new Object[]{ (Function<Object, Object>) BigIntegers::toBigInteger, new Object() },
                              new Object[]{ (Function<Object, Object>) BigDecimals::toBigDecimal, "invalid" },
                              new Object[]{ (Function<Object, Object>) BigDecimals::toBigDecimal, new Object() },
                              new Object[]{ (Function<Object, Object>) Floats::toFloat, "invalid" },
                              new Object[]{ (Function<Object, Object>) Floats::toFloat, new Object() },
                              new Object[]{ (Function<Object, Object>) Doubles::toDouble, "invalid" },
                              new Object[]{ (Function<Object, Object>) Doubles::toDouble, new Object() }



                              //
                            );
    }

    private Function<Object, Object> function;
    private Object                   input;

    public Casts_toXXXX_Fail_Test( Function<Object, Object> function, Object input ) {
        this.function = function;
        this.input = input;
    }

    @Test( expected = Exception.class )
    public void testFromObject() throws Exception {
        // Should explode
        function.apply( input );
    }
}
