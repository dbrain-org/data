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

/**
 * Created by epoitras on 09/10/14.
 */
public class Fqns {

    public static String encodeSegment( String segment ) {
        if ( segment.length() == 0 ) {
            return "''";
        };
        StringBuilder sb = null;
        for ( int i = 0; i < segment.length(); i++ ) {
            Character c = segment.charAt( i );
            if ( ".*\'".indexOf( c ) >= 0 ) {
                if ( sb == null ) {
                    sb = new StringBuilder( segment.length() + 12 );
                    sb.append( "'" );
                    sb.append( segment.substring( 0, i ) );
                }
                if ( c == '\'') {
                    sb.append( "''" );
                } else {
                    sb.append( c );
                }
            } else {
                if ( sb != null ) {
                    sb.append( c );
                }
            }
        }

        if ( sb != null ) {
            sb.append( "\'" );
            return sb.toString();
        } else {
            return segment;
        }
    }


}
