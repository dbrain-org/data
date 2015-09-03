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

import org.dbrain.data.Path;
import org.dbrain.data.PathPattern;

/**
 * Implementation of the Path Pattern.
 */
public class PathPatternImpl implements PathPattern {

    // Empty pattern singleton.
    public static final PathPattern EMPTY_PATTERN = new PathPatternImpl( null, 0 );

    private Node  root;
    private int   partCount;
    private Specs specs;

    public PathPatternImpl( Node root, int partCount ) {
        this.root = root;
        this.partCount = partCount;
    }

    @Override
    public MatchResult match( Path path ) {
        if ( root == null ) {
            if ( path == null || path.size() == 0 ) {
                return new MatchResultImpl( true, partCount );
            } else {
                return new MatchResultImpl( false, partCount );
            }
        }
        MatchResultImpl result = new MatchResultImpl( false, partCount );
        root.match( path, 0, result );
        return result;
    }

    @Override
    public Specs getSpecs() {
        if ( specs == null ) {
            if ( root == null ) {
                specs = new SpecsImpl( Type.EXACT_MATCH, Path.empty() );
            }
            Path.Builder scope = Path.newBuilder();
            Node node = root;
            while ( node != null ) {
                if ( node instanceof SpecificAttribute ) {
                    scope.attr( ( (SpecificAttribute) node ).getAttr() );
                } else if ( node instanceof SpecificIndex ) {
                    scope.index( ( (SpecificIndex) node ).getIndex() );
                } else {
                    break;
                }

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
     * Denote a node that returns a part in the match result.
     */
    public interface PartMatchingNode {

        void setPartIdx( int partIdx );

    }

    /**
     * Default implementation of the matchresult.
     */
    static class MatchResultImpl implements MatchResult {

        private boolean matched;
        private int     partCount;
        private Path[]  parts;

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
        public Path getPart( int idx ) {
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

        void setPart( int idx, Path part ) {
            if ( parts == null ) {
                parts = new Path[partCount];
            }
            parts[idx] = part;
        }

    }

    static class SpecsImpl implements Specs {

        private final Type type;
        private final Path scope;

        public SpecsImpl( Type type, Path scope ) {
            this.type = type;
            this.scope = scope;
        }

        @Override
        public Type getType() {
            return type;
        }

        @Override
        public Path scope() {
            return scope;
        }
    }

    public static abstract class Node {

        protected Node next;

        abstract boolean match( Path fqn, int i, MatchResultImpl mr );

        Node getNext() {
            return next;
        }

        void setNext( Node next ) {
            this.next = next;
        }
    }

    public static class SpecificAttribute extends Node {

        private String attr;

        public SpecificAttribute( String attr ) {
            this.attr = attr;
        }

        public String getAttr() {
            return attr;
        }

        @Override
        boolean match( Path path, int i, MatchResultImpl mr ) {
            if ( i >= path.size() || !( path.nodeType( i ) == Path.NodeType.ATTRIBUTE && path.attr( i )
                                                                                             .equals( attr ) ) ) {
                return mr.answer( false );
            }
            boolean result;
            if ( next != null ) {
                result = next.match( path, i + 1, mr );
            } else {
                result = i == path.size() - 1;
            }
            return mr.answer( result );
        }

        @Override
        public String toString() {
            return PathUtils.encodeAttribute( attr );
        }
    }

    public static class SpecificIndex extends Node {

        private long index;

        public SpecificIndex( long index ) {
            this.index = index;
        }

        public long getIndex() {
            return index;
        }

        @Override
        boolean match( Path path, int i, MatchResultImpl mr ) {
            if ( i >= path.size() || !( path.nodeType( i ) == Path.NodeType.INDEX && path.index( i ) == index ) ) {
                return mr.answer( false );
            }
            boolean result;
            if ( next != null ) {
                result = next.match( path, i + 1, mr );
            } else {
                result = i == path.size() - 1;
            }
            return mr.answer( result );
        }

        @Override
        public String toString() {
            return "[" + index + "]";
        }
    }


    public static class OneNode extends Node implements PartMatchingNode {

        int partIdx;

        @Override
        boolean match( Path path, int i, MatchResultImpl mr ) {
            if ( i >= path.size() ) {
                return mr.answer( false );
            }
            boolean result;
            if ( next != null ) {
                result = next.match( path, i + 1, mr );
            } else {
                result = i == path.size() - 1;
            }
            if ( result ) {
                mr.setPart( partIdx, Path.from( path, i, i + 1 ).build() );
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
        boolean match( Path path, int i, MatchResultImpl mr ) {
            if ( i > path.size() ) {
                return mr.answer( false );
            }
            int j = path.size();
            if ( next != null ) {
                for (; j >= i; j-- ) {
                    if ( next.match( path, j, mr ) ) {
                        break;
                    }
                }
            }
            boolean result = j >= i;
            if ( result ) {
                Path.Builder part = Path.newBuilder();
                for ( int x = i; x < j; x++ ) {
                    part.append( path, x, x + 1 );
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
