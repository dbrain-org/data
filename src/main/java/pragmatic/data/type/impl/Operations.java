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

import java.util.Comparator;
import java.util.Date;

/**
 * Defines type operations interfaces.
 */
public class Operations {

    /**
     * Casting operation.
     */
    public static interface Cast<B> {

        /**
         * Retrieve the base class that this operation will coalesce to.
         */
        Class<B> getBaseClass();

        /**
         * Cast the object to class B.
         */
        B cast( Object value );

    }

    /**
     * Basic operations.
     */
    public static interface Basic<B> extends Comparator<B> {

        /**
         * Compare two values for equity
         *
         * @return true if value1 == value2
         */
        boolean equals( B value1, B value2 );

        /**
         * Compare compare two to base values.
         *
         * @return < 0 if base1 < base2, 0 if base1 = base2, > 0 if base1 > base2
         */
        int compare( B value1, B value2 );

        /**
         * Cast the specified value to string.
         */
        java.lang.String toString( B value );
    }

    /**
     * Numerical type meta-interface.
     */
    public static interface Numeric<B> extends Basic<B> {}

    /**
     * Boolean meta-type..
     */
    public static interface Boolean<B> extends Basic<B> {}

    /**
     * String type meta-interface.
     */
    public static interface String<B> extends Basic<B> {

    }

    /**
     * Date/Time support for type definitions.
     */
    public static interface Temporal<B> extends Basic<B> {

        /**
         * Convert the current date to java type Date.
         */
        Date toDate( B value );

    }

    /**
     * Entity meta-type.
     */
    public static interface Entity<B> {

        /**
         * Retrieve a property contained in the current object.
         *
         * @return the sub-property of the current object.
         */
        Object getProperty( B value, java.lang.String propertyName );

        /**
         * Retrieve an LValue interface to allow assigning the property.
         *
         * @return An LValue.
         */
        LValue getPropertyLValue( B value, java.lang.String propertyName );

    }

    /**
     * Vector meta-type.
     */
    public static interface Vector<B> {

        /**
         * Get the element from an array.
         *
         * @return The element retrieved.
         */
        Object getArrayElement( B value, int index );

        /**
         * Retrieve an LValue interface to allow assigning the array.
         *
         * @return An LValue.
         */
        LValue getArrayElementLValue( B value, int index );


        /**
         * Get the size of an array type.
         *
         * @return The number of elements in the array.
         */
        int getArraySize( B value );

    }

    /**
     * Interface that represent an assignable entity.
     *
     * @author PoitraE
     */
    public static interface LValue {

        public void setValue( Object value );

    }
}
