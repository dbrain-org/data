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

package org.dbrain.data.type.decimal;

import org.dbrain.data.Casts;
import org.dbrain.data.Formatter;
import org.dbrain.data.formats.Formats;
import org.dbrain.data.type.AbstractDataType;
import org.dbrain.data.util.Functions;
import org.dbrain.data.util.Numbers;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Comparator;
import java.util.function.Function;

/**
 * Decimal data type.
 */
public class DecimalType extends AbstractDataType<BigDecimal> {

    private static final Comparator<BigDecimal> NATURAL_ORDER = new Comparator<BigDecimal>() {
        @Override
        public int compare( BigDecimal o1, BigDecimal o2 ) {
            // Handle Null values
            if ( o1 == null ) {
                if ( o2 == null ) return 0;
                else return -1;
            }
            if ( o2 == null ) return 1;

            return o1.compareTo( o2 );
        }
    };
    private final Function<Object, BigDecimal> castFunction;

    /**
     * Create a new numeric type.
     */
    public DecimalType( Integer scale ) {
        this( null, scale, RoundingMode.HALF_UP );

    }

    public DecimalType( Integer precision, Integer scale ) {
        this( precision, scale, RoundingMode.HALF_UP );
    }

    public DecimalType( Integer precision, Integer scale, RoundingMode roundingMode ) {
        castFunction = Functions.compose( Casts::toBigDecimal,
                                          Numbers.setDecimalParameters( precision, scale, roundingMode ) );
    }

    @Override
    public Formatter<? super BigDecimal> getDisplayFormatter() {
        return Formats.TO_STRING;
    }

    @Override
    public Comparator<? super BigDecimal> getComparator() {
        return NATURAL_ORDER;
    }

    @Override
    public Function<Object, ? extends BigDecimal> getCastFunction() {
        return castFunction;
    }

}
