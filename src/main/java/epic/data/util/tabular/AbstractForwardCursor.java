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

import epic.data.Adapter;
import epic.data.adapters.ObjectAdapters;
import epic.data.ForwardCursor;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 12/02/13
 * Time: 9:13 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractForwardCursor implements ForwardCursor {

    @Override
    public <T> T get( int fieldIndex, Adapter<Object, T> adapter ) {
        return adapter.apply( get( fieldIndex ) );
    }

    @Override
    public <T> T get( String fieldName, Adapter<Object, T> adapter ) {
        return adapter.apply( get( fieldName ) );
    }

    @Override
    public Byte getByte( int fieldIndex ) {
        return ObjectAdapters.TO_BYTE.apply( get( fieldIndex ) );
    }

    @Override
    public Byte getByte( String fieldName ) {
        return ObjectAdapters.TO_BYTE.apply( get( fieldName ) );
    }

    @Override
    public <T> T getByte( int fieldIndex, Adapter<? super Byte, T> adapter ) {
        return adapter.apply( getByte( fieldIndex ) );
    }

    @Override
    public <T> T getByte( String fieldName, Adapter<? super Byte, T> adapter ) {
        return adapter.apply( getByte( fieldName ) );
    }

    @Override
    public Short getShort( int fieldIndex ) {
        return ObjectAdapters.TO_SHORT.apply( get( fieldIndex ) );
    }

    @Override
    public Short getShort( String fieldName ) {
        return ObjectAdapters.TO_SHORT.apply( get( fieldName ) );
    }

    @Override
    public <T> T getShort( int fieldIndex, Adapter<? super Short, T> adapter ) {
        return adapter.apply( getShort( fieldIndex ) );
    }

    @Override
    public <T> T getShort( String fieldName, Adapter<? super Short, T> adapter ) {
        return adapter.apply( getShort( fieldName ) );
    }

    @Override
    public Integer getInt( int fieldIndex ) {
        return ObjectAdapters.TO_INTEGER.apply( get( fieldIndex ) );
    }

    @Override
    public Integer getInt( String fieldName ) {
        return ObjectAdapters.TO_INTEGER.apply( get( fieldName ) );
    }

    @Override
    public <T> T getInt( int fieldIndex, Adapter<? super Integer, T> adapter ) {
        return adapter.apply( getInt( fieldIndex ) );
    }

    @Override
    public <T> T getInt( String fieldName, Adapter<? super Integer, T> adapter ) {
        return adapter.apply( getInt( fieldName ) );
    }

    @Override
    public Long getLong( int fieldIndex ) {
        return ObjectAdapters.TO_LONG.apply( getLong( fieldIndex ) );
    }

    @Override
    public Long getLong( String fieldName ) {
        return ObjectAdapters.TO_LONG.apply( getLong( fieldName ) );
    }

    @Override
    public <T> T getLong( int fieldIndex, Adapter<? super Long, T> adapter ) {
        return adapter.apply( getLong( fieldIndex ) );
    }

    @Override
    public <T> T getLong( String fieldName, Adapter<? super Long, T> adapter ) {
        return adapter.apply( getLong( fieldName ) );
    }

    @Override
    public Float getFloat( int fieldIndex ) {
        return ObjectAdapters.TO_FLOAT.apply( get( fieldIndex ) );
    }

    @Override
    public Float getFloat( String fieldName ) {
        return ObjectAdapters.TO_FLOAT.apply( get( fieldName ) );
    }

    @Override
    public <T> T getFloat( int fieldIndex, Adapter<? super Float, T> adapter ) {
        return adapter.apply( getFloat( fieldIndex ) );
    }

    @Override
    public <T> T getFloat( String fieldName, Adapter<? super Float, T> adapter ) {
        return adapter.apply( getFloat( fieldName ) );
    }

    @Override
    public Double getDouble( int fieldIndex ) {
        return ObjectAdapters.TO_DOUBLE.apply( get( fieldIndex ) );
    }

    @Override
    public Double getDouble( String fiendName ) {
        return ObjectAdapters.TO_DOUBLE.apply( get( fiendName ) );
    }

    @Override
    public <T> T getDouble( int fieldIndex, Adapter<? super Double, T> adapter ) {
        return adapter.apply( getDouble( fieldIndex ) );
    }

    @Override
    public <T> T getDouble( String fieldName, Adapter<? super Double, T> adapter ) {
        return adapter.apply( getDouble( fieldName ) );
    }

    @Override
    public Boolean getBoolean( int fieldIndex ) {
        return ObjectAdapters.TO_BOOLEAN.apply( get( fieldIndex ) );
    }

    @Override
    public Boolean getBoolean( String fieldName ) {
        return ObjectAdapters.TO_BOOLEAN.apply( get( fieldName ) );
    }

    @Override
    public <T> T getBoolean( int fieldIndex, Adapter<? super Boolean, T> adapter ) {
        return adapter.apply( getBoolean( fieldIndex ) );
    }

    @Override
    public <T> T getBoolean( String fieldName, Adapter<? super Boolean, T> adapter ) {
        return adapter.apply( getBoolean( fieldName ) );
    }

    @Override
    public String getString( int fieldIndex ) {
        return ObjectAdapters.TO_STRING.apply( get( fieldIndex ) );
    }

    @Override
    public String getString( String fieldName ) {
        return ObjectAdapters.TO_STRING.apply( get( fieldName ) );
    }

    @Override
    public <T> T getString( int fieldIndex, Adapter<? super String, T> adapter ) {
        return adapter.apply( getString( fieldIndex ) );
    }

    @Override
    public <T> T getString( String fieldName, Adapter<? super String, T> adapter ) {
        return adapter.apply( getString( fieldName ) );
    }
}
