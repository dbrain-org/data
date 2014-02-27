/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package epic.data.util;

import epic.data.util.Objects;
import junit.framework.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

/**
 * Test the Object to Number adapters.
 */
public class TestObjectToNumbersAdapters {

    /**
     * Test that number conversion works fine.
     */
    private <T extends Number> void testObjectToNumberAdaptation( Function<Object, T> function, Number numberValue, T expected ) {
        BigDecimal value = new BigDecimal( numberValue.toString() );

        Assert.assertEquals( function.apply( value.toString() ), expected );
        if ( value.unscaledValue().toString().length() <= 9 ) {
            Assert.assertEquals( function.apply( value.floatValue() ), expected );
        }
        if ( value.unscaledValue().toString().length() <= 18 ) {
            Assert.assertEquals( function.apply( value.doubleValue() ), expected );
        }
        Assert.assertEquals( function.apply( new BigDecimal( value.toString() ) ), expected );
        if ( !( numberValue instanceof Float || numberValue instanceof Double ) ) {
            Assert.assertEquals( function.apply( new BigInteger( value.toString() ) ), expected );
        }
        if ( value.compareTo( BigDecimal.valueOf( Long.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Long.MAX_VALUE ) ) <= 0 ) {
            Assert.assertEquals( function.apply( value.longValue() ), expected );
            Assert.assertEquals( function.apply( new AtomicLong( value.longValue() ) ), expected );
        }
        if ( value.compareTo( BigDecimal.valueOf( Integer.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Integer.MAX_VALUE ) ) <= 0 ) {
            Assert.assertEquals( function.apply( value.intValue() ), expected );
            Assert.assertEquals( function.apply( new AtomicInteger( value.intValue() ) ), expected );
        }
        if ( value.compareTo( BigDecimal.valueOf( Short.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Short.MAX_VALUE ) ) <= 0 ) {
            Assert.assertEquals( function.apply( value.shortValue() ), expected );
        }
        if ( value.compareTo( BigDecimal.valueOf( Byte.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Byte.MAX_VALUE ) ) <= 0 ) {
            Assert.assertEquals( function.apply( value.byteValue() ), expected );
        }

    }

    private <T extends Number> boolean testOneNumberAdaptationFailure( Function<Object, T> function, Object value ) {
        try {
            function.apply( value );
        } catch ( Exception e ) {
            return true;
        }
        return false;
    }

    private <T extends Number> void testNumberAdaptationFailure( Function<Object, T> function, Number numberValue ) {
        BigDecimal value = new BigDecimal( numberValue.toString() );

        Assert.assertTrue( testOneNumberAdaptationFailure( function, value.toString() ) );
        if ( value.unscaledValue().toString().length() < 18 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.doubleValue() ) );
        }
        if ( value.unscaledValue().toString().length() < 9 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.floatValue() ) );
        }
        Assert.assertTrue( testOneNumberAdaptationFailure( function, value ) );
        Assert.assertTrue( testOneNumberAdaptationFailure( function, value.toBigInteger() ) );
        if ( value.compareTo( BigDecimal.valueOf( Long.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Long.MAX_VALUE ) ) <= 0 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.longValue() ) );
            Assert.assertTrue( testOneNumberAdaptationFailure( function, new AtomicLong( value.longValue() ) ) );
        }
        if ( value.compareTo( BigDecimal.valueOf( Integer.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Integer.MAX_VALUE ) ) <= 0 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.intValue() ) );
            Assert.assertTrue( testOneNumberAdaptationFailure( function, new AtomicInteger( value.intValue() ) ) );
        }
        if ( value.compareTo( BigDecimal.valueOf( Short.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Short.MAX_VALUE ) ) <= 0 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.shortValue() ) );
        }
        if ( value.compareTo( BigDecimal.valueOf( Byte.MIN_VALUE ) ) >= 0 && value.compareTo( BigDecimal.valueOf( Byte.MAX_VALUE ) ) <= 0 ) {
            Assert.assertTrue( testOneNumberAdaptationFailure( function, value.byteValue() ) );
        }
    }


    @Test
    public void testObjectToByteAdapter() {

        testObjectToNumberAdaptation( Objects::toByte, Byte.MIN_VALUE, Byte.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toByte, Byte.MAX_VALUE, Byte.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toByte, (byte) 0, (byte) 0 );
        testNumberAdaptationFailure( Objects::toByte, -500 );

    }

    @Test
    public void testObjectToShortAdapter() {

        testObjectToNumberAdaptation( Objects::toShort, Short.MIN_VALUE, Short.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toShort, Short.MAX_VALUE, Short.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toShort, (short) 0, (short) 0 );
        testNumberAdaptationFailure( Objects::toShort, -500000 );

    }

    @Test
    public void testObjectToIntegerAdapter() {

        testObjectToNumberAdaptation( Objects::toInteger, Integer.MIN_VALUE, Integer.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toInteger, Integer.MAX_VALUE, Integer.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toInteger, 0, 0 );
        testNumberAdaptationFailure( Objects::toInteger, Long.MIN_VALUE );

    }

    @Test
    public void testObjectToLongAdapter() {

        testObjectToNumberAdaptation( Objects::toLong, Long.MIN_VALUE, Long.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toLong, Long.MAX_VALUE, Long.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toLong, 0L, 0L );
        testNumberAdaptationFailure( Objects::toLong, new BigDecimal( Long.MAX_VALUE ).add( BigDecimal.ONE ) );

    }

    @Test
    public void testObjectToFloatAdapter() {

        testObjectToNumberAdaptation( Objects::toFloat, (float) Long.MIN_VALUE, (float) Long.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toFloat, (float) Long.MAX_VALUE, (float) Long.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toFloat, 0f, 0f );
        testObjectToNumberAdaptation( Objects::toFloat, new BigDecimal( Long.MAX_VALUE ).add( BigDecimal.ONE ), new BigDecimal( Long.MAX_VALUE ).add( BigDecimal.ONE ).floatValue() );

    }

    @Test
    public void testObjectToDoubleAdapter() {

        testObjectToNumberAdaptation( Objects::toDouble, (double) Long.MIN_VALUE, (double) Long.MIN_VALUE );
        testObjectToNumberAdaptation( Objects::toDouble, (double) Long.MAX_VALUE, (double) Long.MAX_VALUE );
        testObjectToNumberAdaptation( Objects::toDouble, 0d, 0d );
        testObjectToNumberAdaptation( Objects::toDouble, new BigDecimal( Long.MAX_VALUE ).add( BigDecimal.ONE ), new BigDecimal( Long.MAX_VALUE ).add( BigDecimal.ONE ).doubleValue() );

    }

    @Test
    public void testObjectToBigDecimalAdapter() {

        testObjectToNumberAdaptation( Objects::toBigDecimal, BigDecimal.ONE, BigDecimal.ONE );
        testObjectToNumberAdaptation( Objects::toBigDecimal, BigDecimal.ONE.negate(), BigDecimal.ONE.negate() );
        testObjectToNumberAdaptation( Objects::toBigDecimal, BigDecimal.ZERO, BigDecimal.ZERO );

    }

    @Test
    public void testObjectToBigIntegerAdapter() {

        testObjectToNumberAdaptation( Objects::toBigInteger, BigInteger.ONE, BigInteger.ONE );
        testObjectToNumberAdaptation( Objects::toBigInteger, BigInteger.ONE.negate(), BigInteger.ONE.negate() );
        testObjectToNumberAdaptation( Objects::toBigInteger, BigInteger.ZERO, BigInteger.ZERO );

    }

    @Test
    public void testBigDecimalScientificNotation() throws Exception {
        String test = "123123E-100";
        BigDecimal bdvalue = Objects.toBigDecimal( test );
        Double dvalue = Objects.toDouble( test );


    }
}
