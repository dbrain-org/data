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

import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;

import java.math.BigDecimal;
import java.math.BigInteger;

public class ValueListBuilderImpl implements NodeList.Builder {

    private NodeList building = NodeList.newInstance();

    @Override
    public NodeList.Builder addNull() {
        building.add( Node.nullValue() );
        return this;
    }

    @Override
    public NodeList.Builder add(Byte v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Short v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Integer v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Long v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(BigInteger v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(BigDecimal v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Float v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Double v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(String v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Boolean v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList.Builder add(Node v ) {
        building.add( Node.of( v ) );
        return this;
    }

    @Override
    public NodeList build() {
        try {
            return building;
        } finally {
            building = null;
        }
    }
}
