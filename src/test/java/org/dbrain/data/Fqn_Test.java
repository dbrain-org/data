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

import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by epoitras on 03/10/14.
 */
public class Fqn_Test {

    /**
     * Test the parsing method of and toString at the same time.
     */
    @Test
    public void testOfString() throws Exception {
        Assert.assertNull( Fqn.of( "" ) );
        Assert.assertNull( Fqn.of( " " ) );
        Assert.assertEquals( "test", Fqn.of( "test" ).toString() );
        Assert.assertEquals( "test", Fqn.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", Fqn.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", Fqn.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", Fqn.of( "'test.'" ).toString() );
        Assert.assertEquals( "'test.'.''.'''123'.'123'''", Fqn.of( "'test.'.''.'''123'.'123'''" ).toString() );
    }

    /**
     * Test invalid encoding.
     */
    @Test
    public void testOfString_invalid() throws Exception {
        String[] invalidFqn = { ".", "*", "[", "]", "*", ",", "test. toto" };

        for ( String s : invalidFqn ) {
            try {
                Fqn.of( s );
                throw new IllegalStateException( "Should not encode: " + s );
            } catch ( ParseException e ) {
                // expected.
            }
        }
    }


    /**
     * Test the size method.
     */
    @Test
    public void testSize() throws Exception {
        Assert.assertEquals( 1, Fqn.of( "''" ).size() );
        Assert.assertEquals( 2, Fqn.of( "'test*'.''" ).size() );
        Assert.assertEquals( 3, Fqn.of( "'test'''.'*'.'**'" ).size() );
        Assert.assertEquals( 4, Fqn.of( "1.2.3.4" ).size() );
    }

    /**
     * Test the segment method.
     */
    @Test
    public void testSegment() throws Exception {
        Assert.assertEquals( "test", Fqn.of( "test" ).segment( 0 ) );
        Assert.assertEquals( "", Fqn.of( "''" ).segment( 0 ) );
        Assert.assertEquals( "*", Fqn.of( "test.'*'" ).segment( 1 ) );
        Assert.assertEquals( "", Fqn.of( "test.''" ).segment( 1 ) );
    }
}
