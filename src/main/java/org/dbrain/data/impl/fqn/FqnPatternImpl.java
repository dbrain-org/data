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

import java.util.List;

/**
 * Created by epoitras on 26/11/14.
 */
public class FqnPatternImpl implements FqnPattern {

    private List<Node> nodes;

    @Override
    public MatchResult match( Fqn fqn ) {
        if ( fqn == null && nodes == null ) {
            // TODO
            return null;
        }
        //TODO
        return null;
    }

    static class MatchResultImpl implements MatchResult {

        @Override
        public boolean matched() {
            return false;
        }

        @Override
        public int partCount() {
            return 0;
        }

        @Override
        public Fqn getPart( int idx ) {
            return null;
        }

    }

    public static abstract class Node {

        private Node next;

        boolean matchNext( Fqn fqn, int i ) {
            if ( next != null && i < fqn.size() ) {
                return next.match( fqn, i + 1 );
            } else {
                return true;
            }

        }
        abstract boolean match( Fqn fqn, int i );
        abstract void match( Fqn fqn, int i, MatchResultImpl matchResult );

        public void setNext( Node next ) {
            this.next = next;
        }
    }

//    public static class SpecificNode extends Node {
//
//        @Override
//        void match( Fqn fqn, int i, MatchResultImpl matchResult ) {
//            if ( match( fqn, i ) && matchNext( fqn, i ))
//
//            return false;
//        }
//    }
}
