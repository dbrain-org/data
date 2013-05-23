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
 * Base type for Long values.
 */
public class LongType extends NumericType<Long, Operations.Numeric<Long>> {

    private RawOperations rawOps = new RawOperations();

    /**
     * Create a new Long type.
     * <p/>
     * The native storage for this type is Long.
     */
    protected LongType() {
    }

    @Override
    public Operations.Numeric<Long> getRawOperations() {
        return rawOps;
    }

    @Override
    public Class<Long> getBaseClass() {
        return Long.class;
    }

    @Override
    public Long cast( Object value ) {

        // null returns null
        if ( value == null ) return null;

        // Cast to Long
        value = TypeUtil.objectGetSingle( value );
        if ( value instanceof Number ) return cast( (Number) value );
        if ( value instanceof String ) return cast( (String) value );

        throw new TypeCastException( this, value );
    }

    public Long cast( Number value ) {
        return value == null ? null : value.longValue();
    }

    public Long cast( String value ) {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            try {
                return Long.parseLong( value.trim() );
            } catch ( Exception ex ) {
                throw new TypeCastException( this, value, ex );
            }
        }
    }

    private class RawOperations implements Operations.Numeric<Long> {

        @Override
        public String toString( Long base ) {
            return base.toString();
        }

        @Override
        public boolean equals( Long base1, Long base2 ) {
            return compare( base1, base2 ) == 0;
        }

        @Override
        public int compare( Long base1, Long base2 ) {
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
