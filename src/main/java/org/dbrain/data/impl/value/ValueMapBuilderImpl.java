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

package org.dbrain.data.impl.value;

import org.dbrain.data.Value;
import org.dbrain.data.ValueMap;

import java.math.BigDecimal;
import java.math.BigInteger;

public class ValueMapBuilderImpl implements ValueMap.Builder {

    private ValueMap building = ValueMap.newInstance();

    @Override
    public ValueMap.Builder putNull( String name ) {
        building.put( name, null );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Byte v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Short v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Integer v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Long v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, BigInteger v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, BigDecimal v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Float v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Double v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, String v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Boolean v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap.Builder put( String name, Value v ) {
        building.put( name, Value.of( v ) );
        return this;
    }

    @Override
    public ValueMap build() {
        try {
            return building;
        } finally {
            building = null;
        }

    }
}
