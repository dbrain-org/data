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
public class Casts_ObjectToXXXX_Test {

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList( //
                              // Boolean
                              new Object[]{ (Function) Casts::toBoolean, "true", Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, "1", Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, "1.0", Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, Boolean.TRUE, Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, 1, Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, 1.1, Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, new BigInteger( "1" ), Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, new BigDecimal( "1" ), Boolean.TRUE },
                              new Object[]{ (Function) Casts::toBoolean, "false", Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, Boolean.FALSE, Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, "0", Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, "0.0", Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, 0, Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, 0.0, Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, new BigInteger( "0" ), Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, new BigDecimal( "0" ), Boolean.FALSE },
                              new Object[]{ (Function) Casts::toBoolean, " ", null },
                              new Object[]{ (Function) Casts::toBoolean, "", null },
                              new Object[]{ (Function) Casts::toBoolean, null, null },

                              new Object[]{ (Function) Casts::toByte, new Byte( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new Short( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new Integer( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new Long( (byte) 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new Float( 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new Double( 10 ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new BigInteger( "10" ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, new BigDecimal( "10" ), new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, "10", new Byte( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toByte, " ", null },
                              new Object[]{ (Function) Casts::toByte, "", null },
                              new Object[]{ (Function) Casts::toByte, null, null },

                              new Object[]{ (Function) Casts::toShort, new Byte( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new Short( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new Integer( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new Long( (byte) 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new Float( 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new Double( 10 ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new BigInteger( "10" ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, new BigDecimal( "10" ), new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, "10", new Short( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toShort, " ", null },
                              new Object[]{ (Function) Casts::toShort, "", null },
                              new Object[]{ (Function) Casts::toShort, null, null },

                              new Object[]{ (Function) Casts::toInteger, new Byte( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new Short( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new Integer( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new Long( (byte) 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new Float( 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new Double( 10 ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new BigInteger( "10" ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, new BigDecimal( "10" ), new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, "10", new Integer( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toInteger, " ", null },
                              new Object[]{ (Function) Casts::toInteger, "", null },
                              new Object[]{ (Function) Casts::toInteger, null, null },

                              new Object[]{ (Function) Casts::toLong, new Byte( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new Short( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new Integer( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new Long( (byte) 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new Float( 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new Double( 10 ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new BigInteger( "10" ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, new BigDecimal( "10" ), new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, "10", new Long( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toLong, " ", null },
                              new Object[]{ (Function) Casts::toLong, "", null },
                              new Object[]{ (Function) Casts::toLong, null, null },

                              new Object[]{ (Function) Casts::toBigInteger, new Byte( (byte) 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new Short( (byte) 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new Integer( (byte) 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new Long( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new Float( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new Double( 10 ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new BigInteger( "10" ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, new BigDecimal( "10" ), new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, "10", new BigInteger( "10" ) },
                              new Object[]{ (Function) Casts::toBigInteger, " ", null },
                              new Object[]{ (Function) Casts::toBigInteger, "", null },
                              new Object[]{ (Function) Casts::toBigInteger, null, null },

                              new Object[]{ (Function) Casts::toBigDecimal, new Byte( (byte) 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new Short( (byte) 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new Integer( (byte) 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new Long( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new Float( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new Double( 10 ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new BigDecimal( "10" ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, new BigDecimal( "10" ), new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, "10", new BigDecimal( "10" ) },
                              new Object[]{ (Function) Casts::toBigDecimal, " ", null },
                              new Object[]{ (Function) Casts::toBigDecimal, "", null },
                              new Object[]{ (Function) Casts::toBigDecimal, null, null },
                              
                              new Object[]{ (Function) Casts::toFloat, new Byte( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new Short( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new Integer( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new Long( (byte) 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new Float( 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new Double( 10 ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new BigInteger( "10" ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, new BigDecimal( "10" ), new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, "10", new Float( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toFloat, " ", null },
                              new Object[]{ (Function) Casts::toFloat, "", null },
                              new Object[]{ (Function) Casts::toFloat, null, null },

                              new Object[]{ (Function) Casts::toDouble, new Byte( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new Short( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new Integer( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new Long( (byte) 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new Float( 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new Double( 10 ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new BigInteger( "10" ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, new BigDecimal( "10" ), new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, "10", new Double( (byte) 10 ) },
                              new Object[]{ (Function) Casts::toDouble, " ", null },
                              new Object[]{ (Function) Casts::toDouble, "", null },
                              new Object[]{ (Function) Casts::toDouble, null, null },

                              new Object[]{ (Function) Casts::toString, Integer.class, Integer.class.toString() },
                              new Object[]{ (Function) Casts::toString, null, null }
                              //
                            );
    }

    private Function<Object, Object> function;
    private Object                   expected;
    private Object                   input;

    public Casts_ObjectToXXXX_Test( Function<Object, Object> function, Object input, Object expected ) {
        this.function = function;
        this.expected = expected;
        this.input = input;
    }

    @Test
    public void testFromObject() throws Exception {
        Assert.assertEquals( function.apply( input ), expected );
    }
}
