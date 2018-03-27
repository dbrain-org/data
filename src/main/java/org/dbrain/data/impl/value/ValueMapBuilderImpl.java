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
import org.dbrain.data.tree.NodeMap;

import java.math.BigDecimal;
import java.math.BigInteger;

public class ValueMapBuilderImpl implements NodeMap.Builder {

    private NodeMap building = NodeMap.newInstance();

    @Override
    public NodeMap.Builder putNull(String name ) {
        building.put( name, null );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Byte v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Short v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Integer v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Long v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, BigInteger v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, BigDecimal v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Float v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Double v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, String v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Boolean v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap.Builder put(String name, Node v ) {
        building.put( name, Node.of( v ) );
        return this;
    }

    @Override
    public NodeMap build() {
        try {
            return building;
        } finally {
            building = null;
        }

    }
}
