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

import java.util.Comparator;

/**
 * Abstract type that supports some basic definitions.
 */
public abstract class BasicType<B, OPS extends Operations.Basic<B>> extends RootType<B> implements Operations.Basic<Object>, Comparator<Object> {

    public abstract OPS getRawOperations();

    @Override
    public boolean equals( Object value1, Object value2 ) {
        return getRawOperations().equals( cast( value1 ), cast( value2 ) );
    }

    @Override
    public int compare( Object value1, Object value2 ) {
        return getRawOperations().compare( cast( value1 ), cast( value2 ) );
    }

    @Override
    public String toString( Object value ) {
        return getRawOperations().toString( cast( value ) );
    }

    /**
     * Check if the specified getValue is occurring in the values collection. Return true if so.
     *
     * @param value  The getValue to check for.
     * @param values The values to search for getValue.
     * @return true if getValue is in values.
     */
    public boolean in( Object value, Object... values ) {
        B castedValue = cast( value );
        OPS rawOps = getRawOperations();

        for ( Object current : values ) {
            if ( rawOps.equals( castedValue, cast( current ) ) ) {
                return true;
            }
        }
        return false;
    }

    /**
     * Return true if the getValue is between min and max inclusively.<p>
     *
     * @param value The getValue to test.
     * @param min   The lower bound getValue.
     * @param max   The higher bound getValue.
     * @return True if getValue is between min and max.
     */
    public boolean between( Object value, Object min, Object max ) {
        B castedValue = cast( value );
        OPS rawOps = getRawOperations();
        return ( rawOps.compare( castedValue, cast( min ) ) >= 0 ) && ( rawOps.compare( castedValue, cast( max ) ) <= 0 );
    }

    /**
     * Check for the minimum getValue in the array of object passed as a
     * parameter.
     *
     * @param values The array of values to search into.
     * @return The minimum getValue found.
     */
    public B min( Object... values ) {
        OPS rawOps = getRawOperations();

        B min = null;
        if ( values.length > 0 ) {
            min = cast( values[0] );
            for ( int i = 1; i < values.length; i++ ) {
                B current = cast( values[i] );
                if ( rawOps.compare( current, min ) < 0 ) {
                    min = current;
                }
            }
        }
        return min;
    }

    /**
     * Check for the minimum getValue in the array of object passed as a
     * parameter, ignoring the null values.
     *
     * @param values The array of values to search into.
     * @return The minimum getValue found or null if all values are nulls or the array is empty.
     */
    public B minNotNull( Object... values ) {
        OPS rawOps = getRawOperations();

        B min = null;
        for ( Object currentRaw : values ) {
            B current = cast( currentRaw );

            if ( current != null ) {
                if ( min == null ) {
                    min = current;
                } else {
                    if ( rawOps.compare( current, min ) < 0 ) {
                        min = current;
                    }
                }
            }
        }
        return min;
    }

    /**
     * Check for the maximum getValue in the array of object passed as a
     * parameter.
     *
     * @param values The array of values to search into.
     * @return The maximum getValue found.
     */
    public B max( Object... values ) {
        OPS rawOps = getRawOperations();

        B max = null;
        if ( values.length > 0 ) {
            max = cast( values[0] );
            for ( int i = 1; i < values.length; i++ ) {
                B current = cast( values[i] );
                if ( rawOps.compare( current, max ) > 0 ) {
                    max = current;
                }
            }
        }
        return max;
    }

    /**
     * Check for the maximum getValue in the array of object passed as a
     * parameter, ignoring the null values.
     *
     * @param values The array of values to search into.
     * @return The maximum getValue found or null if all values are null or the array is empty.
     */
    public B maxNotNull( Object... values ) {
        OPS rawOps = getRawOperations();

        B max = null;
        for ( Object currentRaw : values ) {
            B current = cast( currentRaw );
            if ( current != null ) {
                if ( max == null ) {
                    max = current;
                } else {
                    if ( rawOps.compare( current, max ) > 0 ) {
                        max = current;
                    }
                }
            }
        }
        return max;
    }


}
