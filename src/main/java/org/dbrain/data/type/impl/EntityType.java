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
 * Abstract type for entities.
 */
public abstract class EntityType<B, OPS extends Operations.Entity<B>> extends RootType<B> implements Operations.Entity<Object> {

    public abstract OPS getRawOperations();

    @Override
    public Object getProperty( Object value, String propertyName ) {
        return getRawOperations().getProperty( cast( value ), propertyName );
    }

    @Override
    public Operations.LValue getPropertyLValue( Object value, String propertyName ) {
        return getRawOperations().getPropertyLValue( cast( value ), propertyName );
    }
}
