/*
 * Copyright [2016] [Eric Poitras]
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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * A function registry. Will return the function that process the specific message class.
 */
public class FunctionRegistry<T> {

    /**
     * Start building a registry.
     */
    public static final <T> Builder<T> newBuilder( Class<T> baseClass ) {
        return new Builder( baseClass );
    }

    private final Class<T> baseClass;
    private final Map<Class<? extends T>, Function> functionByClass;


    private FunctionRegistry( Class<T> baseClass, Map<Class<? extends T>, Function> nameMap ) {
        this.baseClass = baseClass;
        this.functionByClass = nameMap;
    }

    /**
     * @return The base class for this registry.
     */
    public Class<T> getBaseClass() {
        return baseClass;
    }

    /**
     * @return The function handling the specific class (or superclass)
     */
    @SuppressWarnings( "unchecked" )
    public <U extends T, R> Function<U, R> get( Class<U> clazz ) {
        Objects.requireNonNull( clazz );
        return functionByClass.get( clazz );
    }


    /**
     * Utility class to build new TypeRegistry instance.
     *
     * Note: This type registry builder should not be reused to build multiple TypeRegistry instance. Use individual instance for each required registry.
     */
    public static class Builder<T> {

        private final Class<T> baseClass;
        private final Map<Class<? extends T>, Function> functionByClass = new HashMap<>();

        /**
         * Build a new type registry with only subclasses of the specific Base Class.
         * @param baseClass
         */
        Builder( Class<T> baseClass ) {
            this.baseClass = baseClass;
        }

        /**
         * Register a new function that takes the specific class.
         */
        public <U extends T, R> Builder<T> add( Class<? extends T> clazz, Function<U, R> fun ) {
            Objects.requireNonNull( clazz );
            if ( !baseClass.isAssignableFrom( clazz )) {
                throw new IllegalStateException( clazz + " do not extends " + baseClass );
            }
            functionByClass.put( clazz, fun );
            return this;
        }

        /**
         * @return A new function registry.
         */
        public FunctionRegistry<T> build() {
            return new FunctionRegistry( baseClass, functionByClass );
        }

    }

}