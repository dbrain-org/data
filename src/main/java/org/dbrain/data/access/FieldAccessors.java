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
 * Denote a container of a single field value.
 */
public interface FieldAccessors {

    /**
     * Read the field value as the original object.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    Object getObject();

    default <T> T getAs( Function<Object, T> function ) {
        return function.apply( getObject() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Byte getByte() {
        return Casts.toByte( getObject() );
    }

    default <T> T getByteAs( Function<? super Byte, T> function ) {
        return function.apply( getByte() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Short getShort() {
        return Casts.toShort( getObject() );
    }

    default <T> T getShortAs( Function<? super Short, T> function ) {
        return function.apply( getShort() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Integer getInt() {
        return Casts.toInteger( getObject() );
    }

    default <T> T getIntAs( Function<? super Integer, T> function ) {
        return function.apply( getInt() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Long getLong() {
        return Casts.toLong( getObject() );
    }

    /**
     * Read the field. Use the adapter to transform the object prior to return.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getLongAs( Function<? super Long, T> function ) {
        return function.apply( getLong() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Float getFloat() {
        return Casts.toFloat( getObject() );
    }

    /**
     * Read the field. Use the adapter to transform the object prior to return.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getFloatAs( Function<? super Float, T> function ) {
        return function.apply( getFloat() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Double getDouble() {
        return Casts.toDouble( getObject() );
    }

    /**
     * Read the field. Use the adapter to transform the object prior to return.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getDoubleAs( Function<? super Double, T> function ) {
        return function.apply( getDouble() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Boolean getBoolean() {
        return Casts.toBoolean( getObject() );
    }

    /**
     * Read the field. Use the adapter to transform the object prior to return.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getBooleanAs( Function<? super Boolean, T> function ) {
        return function.apply( getBoolean() );
    }

    /**
     * Read the field.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default String getString() {
        return Casts.toString( getObject() );
    }

    /**
     * Read the field. Use the adapter to transform the object prior to return.
     *
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getStringAs( Function<? super String, T> function ) {
        return function.apply( getString() );
    }
}
