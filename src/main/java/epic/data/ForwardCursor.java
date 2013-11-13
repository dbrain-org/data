/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package epic.data;

/**
 * Allows to read tabular data in a Forward-only manner.
 */
public interface ForwardCursor {

    /**
     * @return true if the cursor is at end-of-file.
     */
    public boolean eof();

    /**
     * Move the cursor to the next row in the resultset.
     *
     * @return true if the cursor is not at end of file.
     */
    public boolean next();

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Object get( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Object get( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getAs( int fieldIndex, Adapter<Object, T> adapter );

    public <T> T getAs( String fieldName, Adapter<Object, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Byte getByte( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Byte getByte( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getByteAs( int fieldIndex, Adapter<? super Byte, T> adapter );

    public <T> T getByteAs( String fieldName, Adapter<? super Byte, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Short getShort( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Short getShort( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getShortAs( int fieldIndex, Adapter<? super Short, T> adapter );

    public <T> T getShortAs( String fieldName, Adapter<? super Short, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Integer getInt( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Integer getInt( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getIntAs( int fieldIndex, Adapter<? super Integer, T> adapter );

    public <T> T getIntAs( String fieldName, Adapter<? super Integer, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Long getLong( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Long getLong( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getLongAs( int fieldIndex, Adapter<? super Long, T> adapter );

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public <T> T getLongAs( String fieldName, Adapter<? super Long, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Float getFloat( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Float getFloat( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getFloatAs( int fieldIndex, Adapter<? super Float, T> adapter );

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public <T> T getFloatAs( String fieldName, Adapter<? super Float, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Double getDouble( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Double getDouble( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getDoubleAs( int fieldIndex, Adapter<? super Double, T> adapter );

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public <T> T getDoubleAs( String fieldName, Adapter<? super Double, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public Boolean getBoolean( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public Boolean getBoolean( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getBooleanAs( int fieldIndex, Adapter<? super Boolean, T> adapter );

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public <T> T getBooleanAs( String fieldName, Adapter<? super Boolean, T> adapter );

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public String getString( int fieldIndex );

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public String getString( String fieldName );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    public <T> T getStringAs( int fieldIndex, Adapter<? super String, T> adapter );

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    public <T> T getStringAs( String fieldName, Adapter<? super String, T> adapter );

}
