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

/**
 * Abstract type that includes vector definitions.
 */
public abstract class VectorType<B, OPS extends Operations.Vector<B>> extends RootType<B> implements Operations.Vector<Object> {

    public abstract Operations.Vector<B> getRawOperations();

    @Override
    public Object getArrayElement( Object value, int index ) {
        return getRawOperations().getArrayElement( cast( value ), index );
    }

    @Override
    public Operations.LValue getArrayElementLValue( Object value, int index ) {
        return getRawOperations().getArrayElementLValue( cast( value ), index );
    }

    @Override
    public int getArraySize( Object value ) {
        return getRawOperations().getArraySize( cast( value ) );
    }

}
