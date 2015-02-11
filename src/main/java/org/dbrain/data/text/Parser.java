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

package org.dbrain.data.text;

/**
 * Simple contract that allows to convert a string representation to a T value.
 *
 * It is correct for an implementation to return a null value for a representation that makes sense.
 */
public interface Parser<T> {

    /**
     * Parse a string value and return an instance of value T.
     *
     * @param value The value to parse.
     *
     * @return T or null.
     *
     * @throws ParseException If a problem occurs parsing the string value.
     */
    T parse( String value ) throws ParseException;

}
