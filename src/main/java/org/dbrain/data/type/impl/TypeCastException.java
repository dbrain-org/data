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
 * Base exception for all exceptions thrown when an object cannot be casted to a specific type.
 */
public class TypeCastException extends IllegalArgumentException {

    protected RootType<?> rootType;

    protected Class<?> fromClass;

    /**
     * Create a new TypeCastException from the provided information.
     */
    public TypeCastException( RootType<?> rootType, Object fromValue ) {
        this( rootType, fromValue, null );
    }

    /**
     * Create a new TypeCastException from the provided information.
     */
    public TypeCastException( RootType<?> rootType, Object fromValue, Throwable cause ) {
        this( String.format( "Cannot coalesce from getValue %s of class %s to %s",
                             fromValue,
                             fromValue != null ? fromValue.getClass() : null,
                             rootType ), rootType, fromValue != null ? fromValue.getClass() : null, cause );
    }

    /**
     * Create a new TypeCastException from the provided information.
     */
    public TypeCastException( RootType<?> rootType, Class<?> fromClass ) {
        this( String.format( "Cannot coalesce from %s to %s", fromClass, rootType ), rootType, fromClass );
    }

    /**
     * Create a new TypeCastException with the provided information.
     */
    public TypeCastException( String message, RootType<?> rootType, Class<?> fromClass, Throwable cause ) {
        super( message, cause );
        this.rootType = rootType;
        this.fromClass = fromClass;
    }

    /**
     * Create a new TypeCastException with the provided information.
     */
    public TypeCastException( String message, RootType<?> rootType, Class<?> fromClass ) {
        super( message );
        this.rootType = rootType;
        this.fromClass = fromClass;
    }

    /**
     * Gets the RootType used in the coalesce.
     */
    public RootType<?> getRootType() {
        return rootType;
    }

    /**
     * Gets the class of the object being casted.
     */
    public Class<?> getFromClass() {
        return fromClass;
    }

}
