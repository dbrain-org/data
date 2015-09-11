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

package org.dbrain.data;

import org.dbrain.data.impl.path.PathBuilderImpl;
import org.dbrain.data.impl.path.PathImpl;
import org.dbrain.data.impl.path.PathParseUtils;
import org.dbrain.data.text.ReaderCursor;

import java.util.Arrays;

/**
 * Describe a data path.
 */
public interface Path {

    /**
     * @return An empty name.
     */
    static Path empty() {
        return PathImpl.EMPTY_PATH;
    }

    /**
     * Create a path from a ReaderCursor.
     */
    static Path of( ReaderCursor c ) {
        return PathParseUtils.parsePath( c );
    }

    /**
     * Create a new path from a String.
     * Expect to works with the output of toString.
     */
    static Path of( String path ) {
         return PathParseUtils.parsePath( path );
    }

    /**
     * Create a new path from the specific attribute.
     */
    static Path ofAttr( String attr ) {
        if ( attr != null ) {
            return new PathImpl( Arrays.asList( attr ) );
        } else {
            return Path.of( (String) null );
        }
    }

    /**
     * Start building a Path from an initial attribute.
     *
     * @return A builder.
     */
    static Builder fromAttr( String attr ) {
        return newBuilder().attr( attr );
    }

    /**
     * Start building a Path from another name.
     */
    static Builder from( Path path ) {
        return newBuilder().append( path );
    }

    /**
     * Start building a Path from another name.
     */
    static Builder from( Path path, int startIdx, int endIdx ) {
        return newBuilder().append( path, startIdx, endIdx );
    }

    /**
     * @return A new builder instance.
     */
    static Builder newBuilder() {
        return new PathBuilderImpl();
    }

    /**
     * @return The number of nodes in the path.
     */
    int size();

    /**
     * @return The type of node at the specified index.
     */
    NodeType nodeType( int i );

    /**
     * @return The attribute node at the specified index.
     */
    String attr( int i );

    /**
     * @return The index node at the specified index.
     */
    long index( int i );

    /**
     * @param toIndex the number of nodes to return.
     *
     * @return The head of the path up to toIndex nodes.
     * @throws IndexOutOfBoundsException if toIndex &gt; than size.
     */
    Path head( int toIndex );

    /**
     * @param size the number of segments to return.
     *
     * @return The head of the fully qualified name up to count segment.
     * @throws IndexOutOfBoundsException if size &gt; than fqn size.
     */
    Path tail( int size );


    /**
     * @param fromIndex the index of the starting segment.
     *
     * @return The tail of the fully qualified name starting at fromIndex.
     * @throws IndexOutOfBoundsException if fromIndex &gt; than size.
     */
    Path tailFrom( int fromIndex );

    /**
     * @return Test if a name starts with another.
     */
    boolean startsWith( Path other );

    /**
     * Describe the node type.
     */
    enum NodeType {

        /**
         * The node is an attribute.
         */
        ATTRIBUTE,

        /**
         * The node is an index within an array.
         */
        INDEX
    }

    /**
     * Builder interface for the Path.
     */
    interface Builder {

        /**
         * Add an index node to the path.
         */
        Builder index( long index );

        /**
         * Add an attribute node to the path.
         *
         * @param attr The attribute name, cannot be null.
         */
        Builder attr( String attr );

        /**
         * Append another path.
         */
        Builder append( Path path );

        /**
         * Append part of another path.
         */
        Builder append( Path path, int startIdx, int endIdx );

        /**
         * Append all segments of a Fully Qualified Name.
         */
        Builder append( Fqn fqn );

        /**
         * @return The path built.
         */
        Path build();
    }

}
