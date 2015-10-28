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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * A registry of association of ids and types.
 */
public class TypeRegistry {

    /**
     * Start building a instance from the specific name function.
     */
    public static Builder newInstance( Function<Class<?>, String> nameFunc ) {
        return new Builder( Object.class, nameFunc );
    }

    /**
     * Start building a instance from the specific name function.
     */
    public static Builder newInstance( Class<?> baseClass, Function<Class<?>, String> nameFunc ) {
        return new Builder( baseClass, nameFunc );
    }


    private final Class<?>              baseClass;
    private final Map<String, Class<?>> classMap;
    private final Map<Class<?>, String> nameMap;

    private TypeRegistry( Class<?> baseClass, Map<String, Class<?>> classMap, Map<Class<?>, String> nameMap ) {
        this.baseClass = baseClass;
        this.classMap = classMap;
        this.nameMap = nameMap;
    }

    public Class<?> getBaseClass() {
        return baseClass;
    }

    public Class<?> getClass( String name ) {
        return classMap.get( name );
    }

    public String getName( Class<?> clazz ) {
        return nameMap.get( clazz );
    }

    /**
     * Builder for a type registry.
     */
    public static class Builder {

        private final Class<?>                   baseClass;
        private final Function<Class<?>, String> getClassNameFunc;
        private final Map<String, Class<?>> classMap = new HashMap<>();
        private final Map<Class<?>, String> nameMap  = new HashMap<>();

        public Builder( Class<?> baseClass, Function<Class<?>, String> getClassNameFunc ) {
            Objects.requireNonNull( baseClass );
            Objects.requireNonNull( getClassNameFunc );
            this.baseClass = baseClass;
            this.getClassNameFunc = getClassNameFunc;
        }

        /**
         * Register a new type in the registry.
         */
        public Builder registerType( Class<?> clazz ) {
            Objects.requireNonNull( clazz );
            if ( !baseClass.isAssignableFrom( clazz ) ) {
                throw new IllegalStateException( clazz.getName() + " not a subclass of " + baseClass.getName() );
            }
            if ( !classMap.containsValue( baseClass ) ) {
                String name = getClassNameFunc.apply( clazz );
                if ( classMap.containsKey( name ) ) {
                    throw new IllegalStateException( "Duplicate name " + name + " for class " + classMap.get( name ).getName() + " and class " + clazz.getName() );
                }

                classMap.put( name, clazz );
                nameMap.put( clazz, name );
            }
            return this;
        }

        /**
         * @return Build a new TypeRegistry.
         */
        public TypeRegistry build() {
            return new TypeRegistry( baseClass, classMap, nameMap );
        }
    }
}
