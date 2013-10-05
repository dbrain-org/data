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

package epic.data.type.impl;

import epic.data.util.Strings;

/**
 * Base type for double values.
 */
public class DoubleType extends NumericType<Double, Operations.Numeric<Double>> implements Operations.Numeric<Object> {

    /**
     * Internal raw definitions.
     */
    private Operations.Numeric<Double> rawOps = new RawOperations();

    /**
     * Create a new Long type.
     */
    protected DoubleType() {
    }

    @Override
    public Operations.Numeric<Double> getRawOperations() {
        return rawOps;
    }

    @Override
    public Class<Double> getBaseClass() {
        return Double.class;
    }

    @Override
    public Double cast( Object value ) {

        // Cast to Double
        value = TypeUtil.objectGetSingle( value );
        if ( value instanceof Number ) return cast( (Number) value );
        if ( value instanceof String ) return cast( (String) value );

        throw new TypeCastException( this, value );

    }

    public Double cast( Number value ) {
        return value == null ? null : value.doubleValue();
    }

    public Double cast( String value ) {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            try {
                return Double.parseDouble( value );
            } catch ( Exception ex ) {
                throw new TypeCastException( this, value, ex );
            }
        }
    }

    private class RawOperations implements Operations.Numeric<Double> {

        @Override
        public String toString( Double base ) {
            return base.toString();
        }

        @Override
        public boolean equals( Double base1, Double base2 ) {
            return compare( base1, base2 ) == 0;
        }

        @Override
        public int compare( Double base1, Double base2 ) {
            // Handle Null values
            if ( base1 == null ) {
                if ( base2 == null ) return 0;
                else return -1;
            }
            if ( base2 == null ) return 1;

            // The comparaison is the difference between the two.
            return base1.compareTo( base2 );
        }
    }

}
