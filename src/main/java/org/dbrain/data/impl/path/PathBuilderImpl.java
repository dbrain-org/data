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

package org.dbrain.data.impl.path;

import org.dbrain.data.Fqn;
import org.dbrain.data.Path;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Builder implementation of Paths.
 */
public class PathBuilderImpl implements Path.Builder {

    private List<Object> nodes;

    @Override
    public PathBuilderImpl attr( String attr ) {
        Objects.requireNonNull( attr );
        if ( nodes == null ) {
            nodes = new ArrayList<>();
        }
        nodes.add( attr );
        return this;
    }

    public PathBuilderImpl index( long index ) {
        if ( nodes == null ) {
            nodes = new ArrayList<>();
        }
        nodes.add( index );
        return this;
    }

    private void appendNode( Path path, int i ) {
        switch ( path.nodeType( i ) ) {
            case INDEX:
                index( path.index( i ) );
                break;
            case ATTRIBUTE:
                attr( path.attr( i ) );
                break;
        }
    }

    @Override
    public Path.Builder append( Path path ) {
        if ( path != null ) {
            for ( int i = 0; i < path.size(); i++ ) {
                appendNode( path, i );
            }
        }
        return this;
    }

    @Override
    public Path.Builder append( Path path, int startIdx, int endIdx ) {
        if ( path != null ) {
            if ( startIdx >= 0 && endIdx >= 0 && startIdx < path.size() && endIdx <= path.size() ) {
                for ( int i = startIdx; i < endIdx; i++ ) {
                    appendNode( path, i );
                }
            } else {
                throw new IndexOutOfBoundsException();
            }

        } else {
            if ( startIdx != 0 || ( endIdx >= 0 && endIdx <= 1 ) ) {
                throw new IndexOutOfBoundsException();
            }
        }
        return this;
    }


    @Override
    public Path.Builder append( Fqn fqn ) {
        if ( fqn != null ) {
            for ( int i = 0; i < fqn.size(); i++ ) {
                attr( fqn.segment( i ) );
            }
        }
        return this;
    }

    @Override
    public Path build() {
        if ( nodes != null ) {
            return new PathImpl( nodes );
        } else {
            return PathImpl.EMPTY_PATH;
        }
    }

}
