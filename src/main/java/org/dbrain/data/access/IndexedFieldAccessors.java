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
 * Denote a container of fields accessible by indexes.
 */
public interface IndexedFieldAccessors {

    /**
     * Read the raw field value at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    Object getObject( int fieldIndex );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getAs( int fieldIndex, Function<Object, T> function ) {
        return function.apply( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Byte getByte( int fieldIndex ) {
        return Casts.toByte( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getByteAs( int fieldIndex, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Short getShort( int fieldIndex ) {
        return Casts.toShort( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getShortAs( int fieldIndex, Function<? super Short, T> function ) {
        return function.apply( getShort( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Integer getInt( int fieldIndex ) {
        return Casts.toInteger( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getIntAs( int fieldIndex, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Long getLong( int fieldIndex ) {
        return Casts.toLong( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getLongAs( int fieldIndex, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Float getFloat( int fieldIndex ) {
        return Casts.toFloat( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getFloatAs( int fieldIndex, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Double getDouble( int fieldIndex ) {
        return Casts.toDouble( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getDoubleAs( int fieldIndex, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Boolean getBoolean( int fieldIndex ) {
        return Casts.toBoolean( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getBooleanAs( int fieldIndex, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default String getString( int fieldIndex ) {
        return Casts.toString( getObject( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getStringAs( int fieldIndex, Function<? super String, T> function ) {
        return function.apply( getString( fieldIndex ) );
    }
}
