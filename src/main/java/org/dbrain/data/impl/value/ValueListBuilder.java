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
import org.dbrain.data.ValueList;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Created by epoitras on 2/10/15.
 */
public class ValueListBuilder implements ValueList.Builder {

    private ValueList building = ValueList.newInstance();

    @Override
    public ValueList.Builder addNull() {
        building.add( Value.nullValue() );
        return this;
    }

    @Override
    public ValueList.Builder add( Byte v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Short v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Integer v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Long v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( BigInteger v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( BigDecimal v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Float v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Double v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( String v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Boolean v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList.Builder add( Value v ) {
        building.add( Value.of( v ) );
        return this;
    }

    @Override
    public ValueList build() {
        try {
            return building;
        } finally {
            building = null;
        }
    }
}
