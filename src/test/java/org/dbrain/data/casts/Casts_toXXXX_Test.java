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

import org.dbrain.data.cast.*;
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
 * Massive casting test of toXXXX functions.
 */
@RunWith( Parameterized.class )
public class Casts_toXXXX_Test {

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              // Boolean
                              new Object[]{ (Function) Booleans::toBoolean, "true", Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, "1", Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, "1.0", Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, Boolean.TRUE, Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, 1, Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, 1.1, Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, new BigInteger( "1" ), Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, new BigDecimal( "1" ), Boolean.TRUE },
                              new Object[]{ (Function) Booleans::toBoolean, "false", Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, Boolean.FALSE, Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, "0", Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, "0.0", Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, 0, Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, 0.0, Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, new BigInteger( "0" ), Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, new BigDecimal( "0" ), Boolean.FALSE },
                              new Object[]{ (Function) Booleans::toBoolean, " ", null },
                              new Object[]{ (Function) Booleans::toBoolean, "", null },
                              new Object[]{ (Function) Booleans::toBoolean, null, null },

                              new Object[]{ (Function) Bytes::toByte, new Byte( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new Short( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new Integer( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new Long( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new Float( 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new Double( 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new BigInteger( "10" ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, new BigDecimal( "10" ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, "10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Bytes::toByte, " ", null },
                              new Object[]{ (Function) Bytes::toByte, "", null },
                              new Object[]{ (Function) Bytes::toByte, null, null },

                              new Object[]{ (Function) Shorts::toShort, new Byte( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new Short( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new Integer( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new Long( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new Float( 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new Double( 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new BigInteger( "10" ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, new BigDecimal( "10" ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, "10", new Short( (byte) 10 ) },
                              new Object[]{ (Function) Shorts::toShort, " ", null },
                              new Object[]{ (Function) Shorts::toShort, "", null },
                              new Object[]{ (Function) Shorts::toShort, null, null },

                              new Object[]{ (Function) Integers::toInteger, new Byte( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new Short( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new Integer( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new Long( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new Float( 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new Double( 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new BigInteger( "10" ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, new BigDecimal( "10" ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, "10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Integers::toInteger, " ", null },
                              new Object[]{ (Function) Integers::toInteger, "", null },
                              new Object[]{ (Function) Integers::toInteger, null, null },

                              new Object[]{ (Function) Longs::toLong, new Byte( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new Short( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new Integer( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new Long( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new Float( 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new Double( 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new BigInteger( "10" ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, new BigDecimal( "10" ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, "10", new Long( (byte) 10 ) },
                              new Object[]{ (Function) Longs::toLong, " ", null },
                              new Object[]{ (Function) Longs::toLong, "", null },
                              new Object[]{ (Function) Longs::toLong, null, null },

                              new Object[]{ (Function) BigIntegers::toBigInteger, new Byte( (byte) 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new Short( (byte) 10 ), new BigInteger( "10" ) },
                              new Object[]{
                                      (Function) BigIntegers::toBigInteger, new Integer( (byte) 10 ), new BigInteger( "10" )
                              },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new Long( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new Float( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new Double( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new BigInteger( "10" ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, new BigDecimal( "10" ), new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, "10", new BigInteger( "10" ) },
                              new Object[]{ (Function) BigIntegers::toBigInteger, " ", null },
                              new Object[]{ (Function) BigIntegers::toBigInteger, "", null },
                              new Object[]{ (Function) BigIntegers::toBigInteger, null, null },

                              new Object[]{ (Function) BigDecimals::toBigDecimal, new Byte( (byte) 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new Short( (byte) 10 ), new BigDecimal( "10" ) },
                              new Object[]{
                                      (Function) BigDecimals::toBigDecimal, new Integer( (byte) 10 ), new BigDecimal( "10" )
                              },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new Long( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new Float( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new Double( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new BigDecimal( "10" ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, new BigDecimal( "10" ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, "10", new BigDecimal( "10" ) },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, " ", null },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, "", null },
                              new Object[]{ (Function) BigDecimals::toBigDecimal, null, null },

                              new Object[]{ (Function) Floats::toFloat, new Byte( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new Short( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new Integer( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new Long( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new Float( 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new Double( 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new BigInteger( "10" ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, new BigDecimal( "10" ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, "10", new Float( (byte) 10 ) },
                              new Object[]{ (Function) Floats::toFloat, " ", null },
                              new Object[]{ (Function) Floats::toFloat, "", null },
                              new Object[]{ (Function) Floats::toFloat, null, null },

                              new Object[]{ (Function) Doubles::toDouble, new Byte( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new Short( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new Integer( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new Long( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new Float( 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new Double( 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new BigInteger( "10" ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, new BigDecimal( "10" ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, "10", new Double( (byte) 10 ) },
                              new Object[]{ (Function) Doubles::toDouble, " ", null },
                              new Object[]{ (Function) Doubles::toDouble, "", null },
                              new Object[]{ (Function) Doubles::toDouble, null, null },

                              new Object[]{ (Function) Strings::toString, Integer.class, Integer.class.toString() },
                              new Object[]{ (Function) Strings::toString, null, null },
                              //

                              // BigDecimal to -> XXX
                              new Object[]{ (Function<BigDecimal, Object>) Bytes::toByte, new BigDecimal( "10" ), (byte) 10 },
                              new Object[]{ (Function<BigDecimal, Object>) Bytes::toByte, null, null },
                              new Object[]{ (Function<BigDecimal, Object>) Shorts::toShort, new BigDecimal( "10" ), (short) 10 },
                              new Object[]{ (Function<BigDecimal, Object>) Shorts::toShort, null, null },
                              new Object[]{ (Function<BigDecimal, Object>) Integers::toInteger, new BigDecimal( "10" ), 10 },
                              new Object[]{ (Function<BigDecimal, Object>) Integers::toInteger, null, null },
                              new Object[]{ (Function<BigDecimal, Object>) Longs::toLong, new BigDecimal( "10" ), 10l },
                              new Object[]{ (Function<BigDecimal, Object>) Longs::toLong, null, null },
                              new Object[]{
                                      (Function<BigDecimal, Object>) BigIntegers::toBigInteger, new BigDecimal( "10" ), new BigInteger(
                                      "10" )
                              },
                              new Object[]{ (Function<BigDecimal, Object>) BigIntegers::toBigInteger, null, null },

                              // Boolean
                              new Object[]{ (Function<String, Object>) Booleans::toBoolean, "true", Boolean.TRUE },
                              new Object[]{ (Function<String, Object>) Booleans::toBoolean, null, null },

                              new Object[]{ (Function<String, Object>) Bytes::toByte, "10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Bytes::toByte, " 10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Bytes::toByte, "10 ", new Byte( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Bytes::toByte, " ", null },
                              new Object[]{ (Function<String, Object>) Bytes::toByte, "", null },
                              new Object[]{ (Function<String, Object>) Bytes::toByte, null, null },

                              new Object[]{ (Function<String, Object>) Shorts::toShort, "10", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Shorts::toShort, " 10", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Shorts::toShort, "10 ", new Short( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Shorts::toShort, " ", null },
                              new Object[]{ (Function<String, Object>) Shorts::toShort, "", null },
                              new Object[]{ (Function<String, Object>) Shorts::toShort, null, null },

                              new Object[]{ (Function<String, Object>) Integers::toInteger, "10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Integers::toInteger, " 10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Integers::toInteger, "10 ", new Integer( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Integers::toInteger, " ", null },
                              new Object[]{ (Function<String, Object>) Integers::toInteger, "", null },
                              new Object[]{ (Function<String, Object>) Integers::toInteger, null, null },

                              new Object[]{ (Function<String, Object>) Longs::toLong, "10", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Longs::toLong, " 10", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Longs::toLong, "10 ", new Long( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Longs::toLong, " ", null },
                              new Object[]{ (Function<String, Object>) Longs::toLong, "", null },
                              new Object[]{ (Function<String, Object>) Longs::toLong, null, null },

                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, "10", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, " 10", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, "10 ", new BigInteger( "10" ) },
                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, " ", null },
                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, "", null },
                              new Object[]{ (Function<String, Object>) BigIntegers::toBigInteger, null, null },

                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, "10", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, " 10", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, "10 ", new BigDecimal( "10" ) },
                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, " ", null },
                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, "", null },
                              new Object[]{ (Function<String, Object>) BigDecimals::toBigDecimal, null, null },

                              new Object[]{ (Function<String, Object>) Floats::toFloat, "10", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Floats::toFloat, " 10", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Floats::toFloat, "10 ", new Float( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Floats::toFloat, " ", null },
                              new Object[]{ (Function<String, Object>) Floats::toFloat, "", null },
                              new Object[]{ (Function<String, Object>) Floats::toFloat, null, null },

                              new Object[]{ (Function<String, Object>) Doubles::toDouble, "10", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Doubles::toDouble, " 10", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Doubles::toDouble, "10 ", new Double( (byte) 10 ) },
                              new Object[]{ (Function<String, Object>) Doubles::toDouble, " ", null },
                              new Object[]{ (Function<String, Object>) Doubles::toDouble, "", null },
                              new Object[]{ (Function<String, Object>) Doubles::toDouble, null, null }
                              //
                            );
    }

    private Function<Object, Object> function;
    private Object                   expected;
    private Object                   input;

    public Casts_toXXXX_Test( Function<Object, Object> function, Object input, Object expected ) {
        this.function = function;
        this.expected = expected;
        this.input = input;
    }

    @Test
    public void testFromObject() throws Exception {
        Assert.assertEquals( function.apply( input ), expected );
    }
}
