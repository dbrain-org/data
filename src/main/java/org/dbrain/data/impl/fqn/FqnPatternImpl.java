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
import org.dbrain.data.FqnPattern;

/**
 * Implementation of the Fqn Pattern.
 */
public class FqnPatternImpl implements FqnPattern {

    // Empty pattern singleton.
    public static final FqnPattern EMPTY_PATTERN = new FqnPatternImpl( null, 0 );

    private Node  root;
    private int   partCount;
    private Specs specs;

    public FqnPatternImpl( Node root, int partCount ) {
        this.root = root;
        this.partCount = partCount;
    }

    @Override
    public MatchResult match( Fqn fqn ) {
        if ( root == null ) {
            if ( fqn == null || fqn.size() == 0 ) {
                return new MatchResultImpl( true, partCount );
            } else {
                return new MatchResultImpl( false, partCount );
            }
        }
        MatchResultImpl result = new MatchResultImpl( false, partCount );
        root.match( fqn, 0, result );
        return result;
    }

    @Override
    public Specs getSpecs() {
        if ( specs == null ) {
            if ( root == null ) {
                specs = new SpecsImpl( Type.EXACT_MATCH, FqnImpl.EMPTY_NAME );
            }
            Fqn.Builder scope = Fqn.newBuilder();
            Node node = root;
            while ( node != null && node instanceof SpecificNode ) {
                scope.segment( ( (SpecificNode) node ).getSegment() );
                node = node.getNext();
            }
            specs = new SpecsImpl( node != null ? Type.PARTIAL : Type.EXACT_MATCH, scope.build() );
        }
        return specs;
    }

    @Override
    public String toString() {
        Node node = root;
        StringBuilder sb = new StringBuilder();
        while ( node != null ) {
            sb.append( node.toString() );
            if ( node.getNext() != null ) {
                sb.append( "." );
            }
            node = node.getNext();
        }
        return sb.toString();
    }

    /**
     * Default implementation of the matchresult.
     */
    static class MatchResultImpl implements MatchResult {

        private boolean matched;
        private int     partCount;
        private Fqn[]   parts;

        public MatchResultImpl( boolean matched, int partCount ) {
            this.matched = matched;
            this.partCount = partCount;
        }

        @Override
        public boolean matched() {
            return matched;
        }

        @Override
        public int partCount() {
            return partCount;
        }

        @Override
        public Fqn getPart( int idx ) {
            if ( parts != null ) {
                return parts[idx];
            } else {
                if ( idx >= 0 && idx < partCount ) {
                    return null;
                } else {
                    throw new IndexOutOfBoundsException();
                }
            }
        }

        boolean answer( boolean value ) {
            matched = value;
            return value;
        }

        void setPart( int idx, Fqn part ) {
            if ( parts == null ) {
                parts = new Fqn[partCount];
            }
            parts[idx] = part;
        }

    }

    static class SpecsImpl implements Specs {

        private final Type type;
        private final Fqn  scope;

        public SpecsImpl( Type type, Fqn scope ) {
            this.type = type;
            this.scope = scope;
        }

        @Override
        public Type getType() {
            return type;
        }

        @Override
        public Fqn scope() {
            return scope;
        }
    }

    public static abstract class Node {

        protected Node next;

        abstract boolean match( Fqn fqn, int i, MatchResultImpl mr );

        Node getNext() {
            return next;
        }

        void setNext( Node next ) {
            this.next = next;
        }
    }

    /**
     * Denote a node that returns a part in the match result.
     */
    public interface PartMatchingNode {

        void setPartIdx( int partIdx );

    }

    public static class SpecificNode extends Node {

        private String segment;

        public SpecificNode( String segment ) {
            this.segment = segment;
        }

        public String getSegment() {
            return segment;
        }

        @Override
        boolean match( Fqn fqn, int i, MatchResultImpl mr ) {
            if ( i >= fqn.size() || !fqn.segment( i ).equals( segment ) ) {
                return mr.answer( false );
            }
            boolean result;
            if ( next != null ) {
                result = next.match( fqn, i + 1, mr );
            } else {
                result = i == fqn.size() - 1;
            }
            return mr.answer( result );
        }

        @Override
        public String toString() {
            return FqnUtils.encodeSegment( segment );
        }
    }

    public static class OneNode extends Node implements PartMatchingNode {

        int partIdx;

        @Override
        boolean match( Fqn fqn, int i, MatchResultImpl mr ) {
            if ( i >= fqn.size() ) {
                return mr.answer( false );
            }
            boolean result;
            if ( next != null ) {
                result = next.match( fqn, i + 1, mr );
            } else {
                result = i == fqn.size() - 1;
            }
            if ( result ) {
                mr.setPart( partIdx, Fqn.fromSegment( fqn.segment( i ) ).build() );
            }
            return mr.answer( result );
        }

        @Override
        public void setPartIdx( int partIdx ) {
            this.partIdx = partIdx;
        }

        @Override
        public String toString() {
            return "*";
        }
    }

    public static class ManyNode extends Node implements PartMatchingNode {

        private int partIdx;

        @Override
        boolean match( Fqn fqn, int i, MatchResultImpl mr ) {
            if ( i > fqn.size() ) {
                return mr.answer( false );
            }
            int j = fqn.size();
            if ( next != null ) {
                for (; j >= i; j-- ) {
                    if ( next.match( fqn, j, mr ) ) {
                        break;
                    }
                }
            }
            boolean result = j >= i;
            if ( result ) {
                Fqn.Builder part = Fqn.newBuilder();
                for ( int x = i; x < j; x++ ) {
                    part.segment( fqn.segment( x ) );
                }
                mr.setPart( partIdx, part.build() );
            }
            return mr.answer( result );
        }

        @Override
        public void setPartIdx( int partIdx ) {
            this.partIdx = partIdx;
        }

        @Override
        public String toString() {
            return "**";
        }

    }


}
