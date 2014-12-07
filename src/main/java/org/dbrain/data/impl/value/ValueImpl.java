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

package org.dbrain.data.impl.value;

import org.dbrain.data.Value;

/**
 * Wrap a boolean value;
 */
public final class ValueImpl implements Value {

    public static final Value TRUE  = new ValueImpl( Boolean.TRUE );
    public static final Value FALSE = new ValueImpl( Boolean.FALSE );


    private final Object value;

    public ValueImpl( Object value ) {
        this.value = value;
    }

    public Object getObject() {
        return value;
    }

    @Override
    public MapImpl asMap() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ListImpl asList() {
        throw new UnsupportedOperationException();
    }

}
