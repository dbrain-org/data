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

package org.dbrain.data.impl.fqn;

import org.dbrain.data.Fqn;

import java.util.ArrayList;
import java.util.List;

/**
 * Builder implementation of Fully Qualified Names.
 */
public class FqnBuilderImpl implements Fqn.Builder {

    private List<String> segments;

    @Override
    public FqnBuilderImpl segment( String segment ) {
        if ( segments == null ) {
            segments = new ArrayList<>();
        }
        segments.add( segment );
        return this;
    }

    @Override
    public Fqn.Builder append( Fqn fqn ) {
        if ( fqn != null ) {
            for ( int i = 0; i < fqn.size(); i++ ) {
                segment( fqn.segment( i ) );
            }
        }
        return this;
    }

    @Override
    public Fqn build() {
        if ( segments != null ) {
            return new FqnImpl( segments );
        } else {
            return FqnImpl.EMPTY_NAME;
        }
    }

}
