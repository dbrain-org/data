package org.dbrain.data;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * A type registry is used to map classes to names and names to classes in Serialization use cases.
 */
public class TypeRegistry<T> {

    /**
     * Start building a registry from the specific baseclass and name provider.
     * @param nameFunc The function responsible of extracting a type name from a class.
     */
    public static final Builder from( Class<?> baseClass, Function<Class<?>, String> nameFunc ) {
        return new Builder( baseClass, nameFunc );
    }


    private final Class<T> baseClass;
    private final Map<String, Class<? extends T>> classMap;
    private final Map<Class<? extends T>, String> nameMap;

    TypeRegistry( Class<T> baseClass, Map<String, Class<? extends T>> classMap, Map<Class<? extends T>, String> nameMap ) {
        this.baseClass = baseClass;
        this.classMap = classMap;
        this.nameMap = nameMap;
    }

    /**
     * @return The base type for this registry.
     */
    public Class<T> getBaseClass() {
        return baseClass;
    }

    /**
     * @return The type having the name, or null if this name is not registered.
     */
    public Class<? extends T> getTypeByName( String name ) {
        Objects.requireNonNull( name );
        return classMap.get( name );
    }

    /**
     * @return The name for the type.
     */
    public String getNameByType( Class<?> clazz ) {
        Objects.requireNonNull( clazz );
        return nameMap.get( clazz );
    }


    /**
     * Utility class to build new TypeRegistry instance.
     *
     * Note: This type registry builder should not be reused to build multiple TypeRegistry instance. Use individual instance for each required registry.
     */
    public static class Builder<T> {

        private final Class<T>                   baseClass;
        private final Function<Class<?>, String> nameFunc;
        private Map<String, Class<? extends T>> classById = new HashMap<>();
        private Map<Class<? extends T>, String> idByClass = new HashMap<>();

        /**
         * Build a new type registry with only subclasses of the specific Base Class.
         */
        Builder( Class<T> baseClass, Function<Class<?>, String> nameFunc ) {
            this.baseClass = baseClass;
            this.nameFunc = nameFunc;
        }

        /**
         * Register a new type for serialization and deserialization.
         */
        public Builder<T> registerType( Class<? extends T> clazz ) {
            Objects.requireNonNull( clazz );
            if ( !idByClass.containsKey( clazz ) ) {
                String name = nameFunc.apply( clazz );
                if ( name == null ) {
                    throw new IllegalStateException( "Class does not provide any type information: " + clazz.getName() );
                } else if ( classById.containsKey( name ) ) {
                    throw new IllegalStateException( "Two classes has same name: " + classById.get( name )
                                                                                              .getName() + " and " + clazz
                            .getName() );
                } else if ( !baseClass.isAssignableFrom( clazz ) ) {
                    throw new IllegalStateException( clazz.getName() + " is not a subclass of the registry's base class: " + baseClass
                            .getName() );
                }

                idByClass.put( clazz, name );
                classById.put( name, clazz );
            }
            return this;
        }

        /**
         * @return A new type registry.
         */
        public TypeRegistry<T> build() {
            return new TypeRegistry( baseClass, classById, idByClass );
        }

    }

}