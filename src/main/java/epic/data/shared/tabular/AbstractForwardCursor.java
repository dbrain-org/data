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

package epic.data.shared.tabular;

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
        return adapter.adapt( get( fieldIndex ) );
    }

    @Override
    public <T> T get( String fieldName, Adapter<Object, T> adapter ) {
        return adapter.adapt( get( fieldName ) );
    }

    @Override
    public Byte getByte( int fieldIndex ) {
        return ObjectAdapters.BYTE.adapt( get( fieldIndex ) );
    }

    @Override
    public Byte getByte( String fieldName ) {
        return ObjectAdapters.BYTE.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getByte( int fieldIndex, Adapter<? super Byte, T> adapter ) {
        return adapter.adapt( getByte( fieldIndex ) );
    }

    @Override
    public <T> T getByte( String fieldName, Adapter<? super Byte, T> adapter ) {
        return adapter.adapt( getByte( fieldName ) );
    }

    @Override
    public Short getShort( int fieldIndex ) {
        return ObjectAdapters.SHORT.adapt( get( fieldIndex ) );
    }

    @Override
    public Short getShort( String fieldName ) {
        return ObjectAdapters.SHORT.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getShort( int fieldIndex, Adapter<? super Short, T> adapter ) {
        return adapter.adapt( getShort( fieldIndex ) );
    }

    @Override
    public <T> T getShort( String fieldName, Adapter<? super Short, T> adapter ) {
        return adapter.adapt( getShort( fieldName ) );
    }

    @Override
    public Integer getInt( int fieldIndex ) {
        return ObjectAdapters.INTEGER.adapt( get( fieldIndex ) );
    }

    @Override
    public Integer getInt( String fieldName ) {
        return ObjectAdapters.INTEGER.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getInt( int fieldIndex, Adapter<? super Integer, T> adapter ) {
        return adapter.adapt( getInt( fieldIndex ) );
    }

    @Override
    public <T> T getInt( String fieldName, Adapter<? super Integer, T> adapter ) {
        return adapter.adapt( getInt( fieldName ) );
    }

    @Override
    public Long getLong( int fieldIndex ) {
        return ObjectAdapters.LONG.adapt( getLong( fieldIndex ) );
    }

    @Override
    public Long getLong( String fieldName ) {
        return ObjectAdapters.LONG.adapt( getLong( fieldName ) );
    }

    @Override
    public <T> T getLong( int fieldIndex, Adapter<? super Long, T> adapter ) {
        return adapter.adapt( getLong( fieldIndex ) );
    }

    @Override
    public <T> T getLong( String fieldName, Adapter<? super Long, T> adapter ) {
        return adapter.adapt( getLong( fieldName ) );
    }

    @Override
    public Float getFloat( int fieldIndex ) {
        return ObjectAdapters.FLOAT.adapt( get( fieldIndex ) );
    }

    @Override
    public Float getFloat( String fieldName ) {
        return ObjectAdapters.FLOAT.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getFloat( int fieldIndex, Adapter<? super Float, T> adapter ) {
        return adapter.adapt( getFloat( fieldIndex ) );
    }

    @Override
    public <T> T getFloat( String fieldName, Adapter<? super Float, T> adapter ) {
        return adapter.adapt( getFloat( fieldName ) );
    }

    @Override
    public Double getDouble( int fieldIndex ) {
        return ObjectAdapters.DOUBLE.adapt( get( fieldIndex ) );
    }

    @Override
    public Double getDouble( String fiendName ) {
        return ObjectAdapters.DOUBLE.adapt( get( fiendName ) );
    }

    @Override
    public <T> T getDouble( int fieldIndex, Adapter<? super Double, T> adapter ) {
        return adapter.adapt( getDouble( fieldIndex ) );
    }

    @Override
    public <T> T getDouble( String fieldName, Adapter<? super Double, T> adapter ) {
        return adapter.adapt( getDouble( fieldName ) );
    }

    @Override
    public Boolean getBoolean( int fieldIndex ) {
        return ObjectAdapters.BOOLEAN.adapt( get( fieldIndex ) );
    }

    @Override
    public Boolean getBoolean( String fieldName ) {
        return ObjectAdapters.BOOLEAN.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getBoolean( int fieldIndex, Adapter<? super Boolean, T> adapter ) {
        return adapter.adapt( getBoolean( fieldIndex ) );
    }

    @Override
    public <T> T getBoolean( String fieldName, Adapter<? super Boolean, T> adapter ) {
        return adapter.adapt( getBoolean( fieldName ) );
    }

    @Override
    public String getString( int fieldIndex ) {
        return ObjectAdapters.STRING.adapt( get( fieldIndex ) );
    }

    @Override
    public String getString( String fieldName ) {
        return ObjectAdapters.STRING.adapt( get( fieldName ) );
    }

    @Override
    public <T> T getString( int fieldIndex, Adapter<? super String, T> adapter ) {
        return adapter.adapt( getString( fieldIndex ) );
    }

    @Override
    public <T> T getString( String fieldName, Adapter<? super String, T> adapter ) {
        return adapter.adapt( getString( fieldName ) );
    }
}
