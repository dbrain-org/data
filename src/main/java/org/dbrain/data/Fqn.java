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

import org.dbrain.data.impl.FqnImpl;
import org.dbrain.data.text.ReaderCursor;

/**
 * Describe a fully qualified name.
 *
 * Syntax allows for wildcards as well as ways of escaping them.
 *
 * ''
 * test
 * test.''
 * test.123.'*'
 * test.123.'**'
 * test.123.'test*'
 */
public interface Fqn {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    static Fqn of( ReaderCursor c ) {
        return FqnImpl.of( c );
    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    static Fqn of( String fqn ) {
        return FqnImpl.of( fqn );
    }

    int size();

    String segment( int i );

    boolean startsWith( Fqn other );

}
