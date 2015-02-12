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
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;

/**
 * Created by epoitras on 19/01/15.
 */
@RunWith( Parameterized.class )
public class Casts_StringToXXXX_Test {

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              // Boolean
                              new Object[]{ (Function<String, Object>) Casts::toBoolean, "true", Boolean.TRUE },
                              new Object[]{ (Function<String, Object>) Casts::toBoolean, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toByte, "10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toByte, " 10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toByte, "10 ", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toByte, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toByte, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toByte, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toShort, "10", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toShort, " 10", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toShort, "10 ", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toShort, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toShort, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toShort, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toInteger, "10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toInteger, " 10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toInteger, "10 ", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toInteger, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toInteger, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toInteger, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toLong, "10", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toLong, " 10", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toLong, "10 ", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toLong, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toLong, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toLong, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, "10", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, " 10", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, "10 ", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toBigInteger, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, "10", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, " 10", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, "10 ", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toBigDecimal, null, null },
                              
                              new Object[]{ (Function<String, Object>) Casts::toFloat, "10", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toFloat, " 10", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toFloat, "10 ", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toFloat, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toFloat, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toFloat, null, null },

                              new Object[]{ (Function<String, Object>) Casts::toDouble, "10", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toDouble, " 10", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toDouble, "10 ", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Casts::toDouble, " ", null },
                              new Object[]{ (Function<String, Object>) Casts::toDouble, "", null },
                              new Object[]{ (Function<String, Object>) Casts::toDouble, null, null }
                              //
                            );
    }

    private Function<Object, Object> function;
    private Object                   expected;
    private String                   input;

    public Casts_StringToXXXX_Test( Function<Object, Object> function, String input, Object expected ) {
        this.function = function;
        this.expected = expected;
        this.input = input;
    }

    @Test
    public void testFromObject() throws Exception {
        Assert.assertEquals( function.apply( input ), expected );
    }
}
