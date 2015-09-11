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

import java.util.ArrayList;
import java.util.List;

/**
 * Implements the Fqn.
 */
public final class PathImpl implements Path {

    // Singleton for no name value
    public static final Path EMPTY_PATH = new PathImpl( null );


    private final List<Object> nodes;

    /**
     * Use the of(xxx) factory.
     */
    public PathImpl( List<Object> nodes ) {
        this.nodes = nodes != null && nodes.size() > 0 ? new ArrayList<>( nodes ) : null;
    }

    /**
     * @return The number of segments in this FQN.
     */
    @Override
    public int size() {
        return nodes != null ? nodes.size() : 0;
    }

    @Override
    public NodeType nodeType( int i ) {
        if ( nodes != null ) {
            return nodes.get( i ) instanceof String ? NodeType.ATTRIBUTE : NodeType.INDEX;
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * Retrieve an index at the specified path index.
     *
     * @param i Index of the node to retrieve.
     *
     * @return A segment.
     */
    @Override
    public long index( int i ) {
        if ( nodes != null ) {
            return ( (Number) nodes.get( i ) ).longValue();
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * Retrieve an attribute at the specified index.
     *
     * @param i Index of the segment to retrieve.
     *
     * @return A segment.
     */
    @Override
    public String attr( int i ) {
        if ( nodes != null ) {
            return (String) nodes.get( i );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    @Override
    public Path head( int toIndex ) {
        if ( toIndex == 0 ) {
            return new PathImpl( null );
        } else if ( nodes != null ) {
            return new PathImpl( nodes.subList( 0, toIndex ) );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    @Override
    public Path tail( int size ) {
        return tailFrom( size() - size );
    }

    @Override
    public Path tailFrom( int fromIndex ) {
        if ( nodes != null ) {
            if ( fromIndex == nodes.size() ) {
                return new PathImpl( null );
            } else {
                return new PathImpl( nodes.subList( fromIndex, nodes.size() ) );
            }
        } else {
            if ( fromIndex != 0 ) {
                throw new IndexOutOfBoundsException();
            } else {
                return new PathImpl( null );
            }
        }
    }

    /**
     * Validate that the current Fqn starts with the other Fqn.
     */
    @Override
    public boolean startsWith( Path other ) {
        if ( other == null || other == this ) {
            return true;
        }
        int otherSize = other.size();
        if ( size() < otherSize ) {
            return false;
        }
        for ( int i = 0; i < otherSize; i++ ) {
            if ( !attr( i ).equals( other.attr( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Path fqn = (Path) o;
        if ( size() != fqn.size() ) {
            return false;
        }
        for ( int i = 0; i < size(); i++ ) {
            if ( !attr( i ).equals( fqn.attr( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return nodes != null ? nodes.hashCode() : 0;
    }

    /**
     * @return The string representation of this Path.
     */
    @Override
    public String toString() {
        final int size = size();
        if ( size == 0 ) {
            return "";
        } else {
            StringBuilder sb = new StringBuilder();
            for ( int i = 0; i < size; i++ ) {
                switch ( nodeType( i ) ) {
                    case ATTRIBUTE:
                        sb.append( PathParseUtils.encodeAttribute( attr( i ), i == 0 ) );
                        break;
                    case INDEX:
                        sb.append( "[" );
                        sb.append( index( i ) );
                        sb.append( "]" );
                        break;
                }
            }
            return sb.toString();
        }
    }

}
