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

package org.dbrain.data.access;

import org.dbrain.data.Casts;

import java.util.function.Function;

/**
 * Denote a container of fields accessible by names.
 */
public interface NamedFieldAccessors {

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    Object getObject( String fieldName );

    default <T> T getAs( String fieldName, Function<Object, T> function ) {
        return function.apply( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Byte getByte( String fieldName ) {
        return Casts.toByte( getObject( fieldName ) );
    }

    default <T> T getByteAs( String fieldName, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Short getShort( String fieldName ) {
        return Casts.toShort( getObject( fieldName ) );
    }

    default <T> T getShortAs( String fieldName, Function<? super Short, T> function ) {
        return function.apply( getShort( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Integer getInt( String fieldName ) {
        return Casts.toInteger( getObject( fieldName ) );
    }

    default <T> T getIntAs( String fieldName, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Long getLong( String fieldName ) {
        return Casts.toLong( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getLongAs( String fieldName, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Float getFloat( String fieldName ) {
        return Casts.toFloat( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getFloatAs( String fieldName, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Double getDouble( String fieldName ) {
        return Casts.toDouble( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getDoubleAs( String fieldName, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Boolean getBoolean( String fieldName ) {
        return Casts.toBoolean( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getBooleanAs( String fieldName, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default String getString( String fieldName ) {
        return Casts.toString( getObject( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getStringAs( String fieldName, Function<? super String, T> function ) {
        return function.apply( getString( fieldName ) );
    }
}
