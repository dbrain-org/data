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
import java.util.function.Function;

/**
 * Created by epoitras on 19/01/15.
 */
@RunWith( Parameterized.class )
public class Casts_ObjectToXXXX_Fail_Test {

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              // Boolean
                              new Object[]{ (Function<Object, Object>) Casts::toBoolean, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toBoolean, new Object() },

                              new Object[]{ (Function<Object, Object>) Casts::toByte, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toByte, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toShort, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toShort, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toInteger, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toInteger, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toLong, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toLong, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toBigInteger, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toBigInteger, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toBigDecimal, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toBigDecimal, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toFloat, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toFloat, new Object() },
                              new Object[]{ (Function<Object, Object>) Casts::toDouble, "invalid" },
                              new Object[]{ (Function<Object, Object>) Casts::toDouble, new Object() }



                              //
                            );
    }

    private Function<Object, Object> function;
    private Object                   input;

    public Casts_ObjectToXXXX_Fail_Test( Function<Object, Object> function, Object input ) {
        this.function = function;
        this.input = input;
    }

    @Test( expected = Exception.class )
    public void testFromObject() throws Exception {
        // Should explode
        function.apply( input );
    }
}
