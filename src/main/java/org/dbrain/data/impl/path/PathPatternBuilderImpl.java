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

import org.dbrain.data.PathPattern;

/**
 * Implementation of the Pattern Builder.
 */
public class PathPatternBuilderImpl implements PathPattern.Builder {

    private int                  partCount = 0;
    private PathPatternImpl.Node root      = null;
    private PathPatternImpl.Node tail      = null;

    private void addNode( PathPatternImpl.Node node ) {
        if ( tail == null ) {
            root = node;
            tail = node;
        } else {
            tail.setNext( node );
            tail = node;
        }
        if ( node instanceof PathPatternImpl.PartMatchingNode ) {
            ( (PathPatternImpl.PartMatchingNode) node ).setPartIdx( partCount++ );
        }
    }

    @Override
    public PathPattern.Builder attr( String attr ) {
        addNode( new PathPatternImpl.SpecificAttribute( attr ) );
        return this;
    }

    @Override
    public PathPattern.Builder index( long index ) {
        addNode( new PathPatternImpl.SpecificIndex( index ) );
        return this;
    }


    @Override
    public PathPattern.Builder one() {
        addNode( new PathPatternImpl.OneNode() );
        return this;
    }

    @Override
    public PathPattern.Builder any() {
        addNode( new PathPatternImpl.ManyNode() );
        return this;
    }

    @Override
    public PathPattern build() {
        try {
            return root != null ? new PathPatternImpl( root, partCount ) : PathPatternImpl.EMPTY_PATTERN;
        } finally {
            root = null;
            tail = null;
            partCount = 0;
        }
    }
}
