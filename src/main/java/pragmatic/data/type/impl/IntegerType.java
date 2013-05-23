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

package pragmatic.data.type.impl;

import pragmatic.data.shared.Strings;

/**
 * Base type for integer values.
 */
public class IntegerType extends NumericType<Integer, Operations.Numeric<Integer>> {

    private RawOperations rawOps = new RawOperations();

    /**
     * Create a new integer type.
     * <p/>
     * The native storage for this type is Integer.
     */
    protected IntegerType() {
    }

    @Override
    public Operations.Numeric<Integer> getRawOperations() {
        return rawOps;
    }

    @Override
    public Class<Integer> getBaseClass() {
        return Integer.class;
    }

    @Override
    public Integer cast( Object value ) {

        // null returns null
        if ( value == null ) return null;

        // Cast to Integer
        value = TypeUtil.objectGetSingle( value );
        if ( value instanceof Number ) return cast( (Number) value );
        if ( value instanceof String ) return cast( (String) value );

        throw new TypeCastException( this, value );
    }

    public Integer cast( Number value ) {
        return value == null ? null : value.intValue();
    }

    public Integer cast( String value ) {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            try {
                return Integer.parseInt( value.trim() );
            } catch ( Exception ex ) {
                throw new TypeCastException( this, value, ex );
            }
        }
    }

    private class RawOperations implements Operations.Numeric<Integer> {

        @Override
        public String toString( Integer base ) {
            return base.toString();
        }

        @Override
        public boolean equals( Integer base1, Integer base2 ) {
            return compare( base1, base2 ) == 0;
        }

        @Override
        public int compare( Integer base1, Integer base2 ) {
            // Handle Null values
            if ( base1 == null ) {
                if ( base2 == null ) return 0;
                else return -1;
            }
            if ( base2 == null ) return 1;

            // The comparaison is the difference between the two.
            int result = base1.compareTo( base2 );
            return result;
        }

    }

}
