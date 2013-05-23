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

/**
 * Contains functions to help implements typing.
 *
 * @author PoitraE
 */
public class TypeUtil {

    /**
     * Disallow instance creation of TypeUtil.
     */
    private TypeUtil() {
    }

    /**
     * Unwrap any array until a single non-array object is found.
     * This function can be used when  unwrapped object is logically
     * equal to the same object wrapped in one or more arrays of 1 element.
     * <p/>
     * Only array of 1 elements are unwrapped and any array with
     * other sizes will throw a ClassCastException error.
     *
     * @param o The object to potentially unwrap.
     * @return The single object with any layer of array removed or null if o is
     *         null or the array contains only a null element.
     */
    static public Object objectGetSingle( Object o ) {
        if ( o != null ) while ( o.getClass().isArray() ) {
            if ( java.lang.reflect.Array.getLength( o ) != 1 ) throw new ClassCastException();
            o = java.lang.reflect.Array.get( o, 0 );
        }
        return o;
    }

}
