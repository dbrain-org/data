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

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * Test the FqnPattern.
 */
public class FqnPatternTest extends TestCase {

    public void testNewBuilder() throws Exception {

        Assert.assertEquals( "test.*.**", FqnPattern.newBuilder().segment( "test" ).one().many().build().toString() );
        Assert.assertEquals( "", FqnPattern.newBuilder().build().toString() );
        Assert.assertEquals( "''", FqnPattern.newBuilder().segment( "" ).build().toString() );

    }

    public void testSimpleMatch() throws Exception {
        FqnPattern pattern;
        FqnPattern.MatchResult matchResult;

        // Test simple pattern
        pattern = FqnPattern.newBuilder().segment( "test" ).build();

        matchResult = pattern.match( Fqn.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Fqn.of( "test.123" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Fqn.of( "Test" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Fqn.of( (String) null ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

    }
}