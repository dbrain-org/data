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

    public <T> T get( int fieldIndex, Adapter<Object, T> adapter );

    public <T> T get( String fieldName, Adapter<Object, T> adapter );

    public Byte getByte( int fieldIndex );

    public Byte getByte( String fieldName );

    public <T> T getByte( int fieldIndex, Adapter<? super Byte, T> adapter );

    public <T> T getByte( String fieldName, Adapter<? super Byte, T> adapter );

    public Short getShort( int fieldIndex );

    public Short getShort( String fieldName );

    public <T> T getShort( int fieldIndex, Adapter<? super Short, T> adapter );

    public <T> T getShort( String fieldName, Adapter<? super Short, T> adapter );

    public Integer getInt( int fieldIndex );

    public Integer getInt( String fieldName );

    public <T> T getInt( int fieldIndex, Adapter<? super Integer, T> adapter );

    public <T> T getInt( String fieldName, Adapter<? super Integer, T> adapter );

    public Long getLong( int fieldindex );

    public Long getLong( String fieldName );

    public <T> T getLong( int fieldIndex, Adapter<? super Long, T> adapter );

    public <T> T getLong( String fieldName, Adapter<? super Long, T> adapter );

    public Float getFloat( int fieldIndex );

    public Float getFloat( String fieldName );

    public <T> T getFloat( int fieldIndex, Adapter<? super Float, T> adapter );

    public <T> T getFloat( String fieldName, Adapter<? super Float, T> adapter );

    public Double getDouble( int fieldIndex );

    public Double getDouble( String fiendName );

    public <T> T getDouble( int fieldIndex, Adapter<? super Double, T> adapter );

    public <T> T getDouble( String fieldName, Adapter<? super Double, T> adapter );

    public Boolean getBoolean( int fieldIndex );

    public Boolean getBoolean( String fieldName );

    public <T> T getBoolean( int fieldIndex, Adapter<? super Boolean, T> adapter );

    public <T> T getBoolean( String fieldName, Adapter<? super Boolean, T> adapter );

    public String getString( int fieldIndex );

    public String getString( String fieldName );

    public <T> T getString( int fieldIndex, Adapter<? super String, T> adapter );

    public <T> T getString( String fieldName, Adapter<? super String, T> adapter );

}
