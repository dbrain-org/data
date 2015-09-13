/*
 * Copyright [2015] [Eric Poitras]
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

import java.io.Reader;
import java.io.Writer;

/**
 * TextSerializer interface.
 */
public interface TextSerializer {

    /**
     * Convert an object to another format by serializing and deserializing it.
     */
    <T> T convert( Object o, Class<T> clazz );

    /**
     * Parse a String to the specific class.
     */
    <T> T read( String from, Class<T> clazz );

    /**
     * Parse a Value to the specific class.
     */
    <T> T read( Reader from, Class<T> clazz );

    /**
     * Serialize an object to a writer.
     */
    void write( Writer to, Object o );

    /**
     * Stream Value to String.
     */
    String writeToString( Object o );

}
