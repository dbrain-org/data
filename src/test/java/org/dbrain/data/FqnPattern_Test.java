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
public class FqnPattern_Test extends TestCase {

    public void testNewBuilder() throws Exception {

        Assert.assertEquals( "test.*.**", FqnPattern.newBuilder().segment( "test" ).one().any().build().toString() );
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

    public void testOneMatch() throws Exception {
        FqnPattern pattern;
        FqnPattern.MatchResult matchResult;

        // Test one pattern only pattern
        pattern = FqnPattern.newBuilder().one().build();

        matchResult = pattern.match( Fqn.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "test.test2" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        // Test one pattern after a specific pattern
        pattern = FqnPattern.newBuilder().segment( "root" ).one().build();

        matchResult = pattern.match( Fqn.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root.test2.test3" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        // Test one pattern after a any pattern
        pattern = FqnPattern.newBuilder().any().one().build();

        matchResult = pattern.match( Fqn.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.root2.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "root.root2" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 1 ) );

        // Test one pattern after a any pattern
        pattern = FqnPattern.newBuilder().one().any().build();

        matchResult = pattern.match( Fqn.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 1 ) );

        // Test one pattern before a any pattern and then a specific segment.
        pattern = FqnPattern.newBuilder().one().any().segment( "test" ).build();

        matchResult = pattern.match( Fqn.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.test2" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );
        Assert.assertNull( matchResult.getPart( 1 ) );
    }

    public void testAnyMatch() throws Exception {
        FqnPattern pattern;
        FqnPattern.MatchResult matchResult;

        // Test any stuck between two static pattern
        pattern = FqnPattern.newBuilder().segment( "root" ).any().segment( "tail" ).build();

        matchResult = pattern.match( Fqn.of( "root.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root.test.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root.test.test2.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test.test2" ), matchResult.getPart( 0 ) );

        // Test any stuck between two static pattern
        pattern = FqnPattern.newBuilder().segment( "root" ).any().any().segment( "tail" ).build();

        matchResult = pattern.match( Fqn.of( "root.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.test.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Fqn.of( "root.test.test2.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test.test2" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 1 ) );

        // Test tail any pattern
        pattern = FqnPattern.newBuilder().segment( "root" ).any().build();

        matchResult = pattern.match( Fqn.of( "root" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root.test1" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test1" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Fqn.of( "root.test1.test2" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Fqn.of( "test1.test2" ), matchResult.getPart( 0 ) );

    }

    public void testOfString() throws Exception {
        Assert.assertEquals( "", FqnPattern.of( "" ).toString() );
        Assert.assertEquals( "", FqnPattern.of( " " ).toString() );
        Assert.assertEquals( "test", FqnPattern.of( "test" ).toString() );
        Assert.assertEquals( "test", FqnPattern.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", FqnPattern.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", FqnPattern.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", FqnPattern.of( "'test.'" ).toString() );
        Assert.assertEquals( "'test.'.''.'''123'.'123'''", FqnPattern.of( "'test.'.''.'''123'.'123'''" ).toString() );
        Assert.assertEquals( "test.*", FqnPattern.of( "test.*" ).toString() );
        Assert.assertEquals( "test.**", FqnPattern.of( "test.**" ).toString() );
        Assert.assertEquals( "test.*", FqnPattern.of( " test.* " ).toString() );
        Assert.assertEquals( "test.**", FqnPattern.of( " test.** " ).toString() );
        Assert.assertEquals( "test.*.test2", FqnPattern.of( "test.*.test2" ).toString() );
        Assert.assertEquals( "test.**.test2", FqnPattern.of( "test.**.test2" ).toString() );

    }
}