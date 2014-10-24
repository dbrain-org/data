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

package org.dbrain.data;

/**
 * Simple contract that provide a String representation from a T value.
 * <p/>
 * Implementation should nicely handle null values and should prefer to return a non-null representation
 * if possible.
 */
public interface Formatter<T> {

    /**
     * @return A string representation for the value T.
     * @throws FormatException In case there is a problem with formatting.
     */
    String format( T value ) throws FormatException;

}
