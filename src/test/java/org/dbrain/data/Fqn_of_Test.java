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
public class Fqn_of_Test {

    /**
     * Validate that the encoding.
     */
    @Test
    public void testSegmentEncoding() throws Exception {
        Assert.assertNull( Fqn.of( "" ) );
        Assert.assertNull( Fqn.of( " " ) );
        Assert.assertEquals( "test", Fqn.of( "test" ).toString() );
        Assert.assertEquals( "test", Fqn.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", Fqn.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", Fqn.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", Fqn.of( "'test.'" ).toString() );
    }

    /**
     * Test invalid encoding.
     */
    @Test
    public void invalidEncoding() throws Exception {
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
}
