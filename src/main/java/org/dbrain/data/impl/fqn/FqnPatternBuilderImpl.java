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

import org.dbrain.data.FqnPattern;

/**
 * Created by epoitras on 28/11/14.
 */
public class FqnPatternBuilderImpl implements FqnPattern.Builder {

    private int partCount = 0;
    private FqnPatternImpl.Node root = null;
    private FqnPatternImpl.Node tail = null;

    private void addNode( FqnPatternImpl.Node node ) {
        if ( tail == null ) {
            root = node;
            tail = node;
        } else {
            tail.setNext( node );
            tail = node;
        }
        if ( node instanceof FqnPatternImpl.PartMatchingNode ) {
            ( (FqnPatternImpl.PartMatchingNode) node ).setPartIdx( partCount++ );
        }
    }

    @Override
    public FqnPattern.Builder segment( String segment ) {
        addNode( new FqnPatternImpl.SpecificNode( segment ) );
        return this;
    }

    @Override
    public FqnPattern.Builder one() {
        addNode( new FqnPatternImpl.OneNode() );
        return this;
    }

    @Override
    public FqnPattern.Builder any() {
        addNode( new FqnPatternImpl.ManyNode() );
        return this;
    }

    @Override
    public FqnPattern build() {
        try {
            return root != null ? new FqnPatternImpl( root, partCount ) : FqnPatternImpl.EMPTY_PATTERN;
        } finally {
            root = null;
            tail = null;
            partCount = 0;
        }
    }
}
