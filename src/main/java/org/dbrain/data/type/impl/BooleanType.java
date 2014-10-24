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

package org.dbrain.data.type.impl;


import org.dbrain.data.util.Strings;

/**
 * Base type for boolean values.
 */
public class BooleanType extends BasicType<Boolean, Operations.Boolean<Boolean>> implements Operations.Boolean<Object> {

    private RawOperations rawOps = new RawOperations();

    /**
     * Create a new boolean.
     */
    protected BooleanType() {
    }

    @Override
    public Operations.Boolean<Boolean> getRawOperations() {
        return rawOps;
    }

    public Class<Boolean> getBaseClass() {
        return Boolean.class;
    }

    public Boolean cast( Object value ) {
        if ( value == null ) {
            return null;
        }

        value = TypeUtil.objectGetSingle( value );
        if ( value instanceof Operations.Boolean ) {
            return (Boolean) value;
        }
        if ( value instanceof String ) {
            return cast( (String) value );
        }
        if ( value instanceof Number ) {
            return cast( (Number) value );
        }

        throw new TypeCastException( this, value );

    }

    /**
     * Cast the string getValue to a boolean getValue. This function consider a null
     * getValue on a null string OR a blank string.
     *
     * @param value The string to coalesce.
     * @return The boolean getValue or null.
     */
    public Boolean cast( String value ) {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            try {
                return new Boolean( value );
            } catch ( Exception ex ) {
                throw new TypeCastException( this, value, ex );
            }

        }
    }

    public Boolean cast( Number value ) {
        if ( value == null ) {
            return null;
        } else {
            return new Boolean( ( value ).longValue() != 0 );
        }
    }

    private class RawOperations implements Operations.Boolean<Boolean> {

        @Override
        public String toString( Boolean base ) {
            if ( base == null ) {
                throw new NullPointerException();
            }
            return base.toString();
        }

        @Override
        public boolean equals( Boolean value1, Boolean value2 ) {
            return compare( value1, value2 ) == 0;
        }

        @Override
        public int compare( Boolean base1, Boolean base2 ) {

            // Handle Null values
            if ( base1 == null ) {
                if ( base2 == null ) {
                    return 0;
                } else {
                    return -1;
                }
            }
            if ( base2 == null ) {
                return 1;
            }

            // Handle else
            return base1.compareTo( base2 );
        }
    }

}
