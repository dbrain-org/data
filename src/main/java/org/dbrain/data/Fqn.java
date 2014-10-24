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

package org.dbrain.data;

import java.util.ArrayList;
import java.util.List;

/**
 * Describe a fully qualified name.
 *
 * Syntax allows for wildcards as well as ways of escaping them.
 *
 * ''
 * test
 * test.''
 * test.123.'*'
 * test.123.'**'
 * test.123.'test*'
 */
public class Fqn {

    private final List<String> segments;

    public Fqn( List<String> segments ) {
        this.segments = segments != null && segments.size() > 0 ? new ArrayList<>( segments ) : null;
    }

    public int getSize() {
        return segments != null ? segments.size() : 0;
    }

    public String getSegment( int i ) {
        if ( segments != null ) {
            return segments.get( i );
        } else {
            throw new IndexOutOfBoundsException();
        }
    }


    public String toString() {
        if ( segments == null || segments.size() == 0 ) {
            return "";
        } else {
            if ( segments.size() == 1 ) {
                return Fqns.encodeSegment( segments.get( 0 ) );
            } else {
                StringBuilder sb = null;
                for ( String s : segments ) {
                    if (sb == null ) {
                        sb = new StringBuilder();
                    } else {
                        sb.append( "." );
                    }
                        sb.append( Fqns.encodeSegment( s ) );
                }
                return sb.toString();
            }
        }
    }

}
