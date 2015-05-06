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
 * Implements the Fqn.
 */
public final class FqnImpl implements Fqn {

    // Singleton for no name value
    public static final Fqn EMPTY_NAME = new FqnImpl( null );


    private final List<String> segments;

    /**
     * Use the of(xxx) factory.
     */
    public FqnImpl( List<String> segments ) {
        this.segments = segments != null && segments.size() > 0 ? new ArrayList<>( segments ) : null;
    }

    /**
     * @return The number of segments in this FQN.
     */
    @Override
    public int size() {
        return segments != null ? segments.size() : 0;
    }

    /**
     * Retrieve the decoded segment.
     *
     * @param i Index of the segment to retrieve.
     *
     * @return A segment.
     */
    @Override
    public String segment( int i ) {
        if ( segments != null ) {
            return segments.get( i );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    @Override
    public Fqn head( int toIndex ) {
        if ( toIndex == 0 ) {
            return new FqnImpl( null );
        } else if ( segments != null ) {
            return new FqnImpl( segments.subList( 0, toIndex ) );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }

    @Override
    public Fqn tail( int size ) {
        return tailFrom( size() - size );
    }

    @Override
    public Fqn tailFrom( int fromIndex ) {
        if ( segments != null ) {
            if ( fromIndex == segments.size() ) {
                return new FqnImpl( null );
            } else {
                return new FqnImpl( segments.subList( fromIndex, segments.size() ) );
            }
        } else {
            if ( fromIndex != 0 ) {
                throw new IndexOutOfBoundsException();
            } else {
                return new FqnImpl( null );
            }
        }
    }

    /**
     * Validate that the current Fqn starts with the other Fqn.
     */
    @Override
    public boolean startsWith( Fqn other ) {
        if ( other == null || other == this ) {
            return true;
        }
        int otherSize = other.size();
        if ( size() < otherSize ) {
            return false;
        }
        for ( int i = 0; i < otherSize; i++ ) {
            if ( !segment( i ).equals( other.segment( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Fqn fqn = (Fqn) o;
        if ( size() != fqn.size() ) {
            return false;
        }
        for ( int i = 0; i < size(); i++ ) {
            if ( !segment( i ).equals( fqn.segment( i ) ) ) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return segments != null ? segments.hashCode() : 0;
    }

    /**
     * @return The string representation of this FQN.
     */
    @Override
    public String toString() {
        final int size = size();
        if ( size == 0 ) {
            return "";
        } else {
            if ( size == 1 ) {
                return FqnUtils.encodeSegment( segment( 0 ) );
            } else {
                StringBuilder sb = null;
                for ( int i = 0; i < size; i++ ) {
                    String s = segment( i );
                    if ( sb == null ) {
                        sb = new StringBuilder();
                    } else {
                        sb.append( "." );
                    }
                    sb.append( FqnUtils.encodeSegment( s ) );
                }
                return sb != null ? sb.toString() : "";
            }
        }
    }

}
