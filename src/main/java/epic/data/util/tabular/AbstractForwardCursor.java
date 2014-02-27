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

package epic.data.util.tabular;

import java.util.function.Function;

import epic.data.ForwardCursor;
import epic.data.util.Objects;

/**
 * Abstract class to simplify implementing concrete versions of ForwardCursor.
 */
public abstract class AbstractForwardCursor implements ForwardCursor {

    @Override
    public <T> T getAs( int fieldIndex, Function<Object, T> function ) {
        return function.apply( get( fieldIndex ) );
    }

    @Override
    public <T> T getAs( String fieldName, Function<Object, T> function ) {
        return function.apply( get( fieldName ) );
    }

    @Override
    public Byte getByte( int fieldIndex ) {
        return Objects.toByte( get( fieldIndex ) );
    }

    @Override
    public Byte getByte( String fieldName ) {
        return Objects.toByte( get( fieldName ) );
    }

    @Override
    public <T> T getByteAs( int fieldIndex, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldIndex ) );
    }

    @Override
    public <T> T getByteAs( String fieldName, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldName ) );
    }

    @Override
    public Short getShort( int fieldIndex ) {
        return Objects.toShort( get( fieldIndex ) );
    }

    @Override
    public Short getShort( String fieldName ) {
        return Objects.toShort( get( fieldName ) );
    }

    @Override
    public <T> T getShortAs( int fieldIndex, Function<? super Short, T> function ) {
        return function.apply( getShort( fieldIndex ) );
    }

    @Override
    public <T> T getShortAs( String fieldName, Function<? super Short, T> function ) {
        return function.apply( getShort( fieldName ) );
    }

    @Override
    public Integer getInt( int fieldIndex ) {
        return Objects.toInteger( get( fieldIndex ) );
    }

    @Override
    public Integer getInt( String fieldName ) {
        return Objects.toInteger( get( fieldName ) );
    }

    @Override
    public <T> T getIntAs( int fieldIndex, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldIndex ) );
    }

    @Override
    public <T> T getIntAs( String fieldName, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldName ) );
    }

    @Override
    public Long getLong( int fieldIndex ) {
        return Objects.toLong( getLong( fieldIndex ) );
    }

    @Override
    public Long getLong( String fieldName ) {
        return Objects.toLong( getLong( fieldName ) );
    }

    @Override
    public <T> T getLongAs( int fieldIndex, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldIndex ) );
    }

    @Override
    public <T> T getLongAs( String fieldName, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldName ) );
    }

    @Override
    public Float getFloat( int fieldIndex ) {
        return Objects.toFloat( get( fieldIndex ) );
    }

    @Override
    public Float getFloat( String fieldName ) {
        return Objects.toFloat( get( fieldName ) );
    }

    @Override
    public <T> T getFloatAs( int fieldIndex, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldIndex ) );
    }

    @Override
    public <T> T getFloatAs( String fieldName, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldName ) );
    }

    @Override
    public Double getDouble( int fieldIndex ) {
        return Objects.toDouble( get( fieldIndex ) );
    }

    @Override
    public Double getDouble( String fieldName ) {
        return Objects.toDouble( get( fieldName ) );
    }

    @Override
    public <T> T getDoubleAs( int fieldIndex, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldIndex ) );
    }

    @Override
    public <T> T getDoubleAs( String fieldName, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldName ) );
    }

    @Override
    public Boolean getBoolean( int fieldIndex ) {
        return Objects.toBoolean( get( fieldIndex ) );
    }

    @Override
    public Boolean getBoolean( String fieldName ) {
        return Objects.toBoolean( get( fieldName ) );
    }

    @Override
    public <T> T getBooleanAs( int fieldIndex, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldIndex ) );
    }

    @Override
    public <T> T getBooleanAs( String fieldName, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldName ) );
    }

    @Override
    public String getString( int fieldIndex ) {
        return Objects.toString( get( fieldIndex ) );
    }

    @Override
    public String getString( String fieldName ) {
        return Objects.toString( get( fieldName ) );
    }

    @Override
    public <T> T getStringAs( int fieldIndex, Function<? super String, T> function ) {
        return function.apply( getString( fieldIndex ) );
    }

    @Override
    public <T> T getStringAs( String fieldName, Function<? super String, T> function ) {
        return function.apply( getString( fieldName ) );
    }
}
