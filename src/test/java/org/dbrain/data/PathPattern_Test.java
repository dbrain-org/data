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

import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test the PathPattern.
 */
public class PathPattern_Test {

    @Test
    public void testNewBuilder() throws Exception {

        Assert.assertEquals( "test.*.**", PathPattern.newBuilder().attr( "test" ).one().any().build().toString() );
        Assert.assertEquals( "", PathPattern.newBuilder().build().toString() );
        Assert.assertEquals( "''", PathPattern.newBuilder().attr( "" ).build().toString() );

    }

    @Test
    public void testEmptyMatch() throws Exception {
        PathPattern pattern;
        PathPattern.MatchResult matchResult;

        // Test simple pattern
        pattern = PathPattern.newBuilder().build();

        matchResult = pattern.match( Path.of( "" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Path.of( "test" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );
    }

    @Test
    public void testSimpleMatch() throws Exception {
        PathPattern pattern;
        PathPattern.MatchResult matchResult;

        // Test simple pattern
        pattern = PathPattern.newBuilder().attr( "test" ).build();

        matchResult = pattern.match( Path.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Path.of( "test.a123" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Path.of( "Test" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

        matchResult = pattern.match( Path.of( (String) null ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 0, matchResult.partCount() );

    }

    @Test
    public void testOneMatch() throws Exception {
        PathPattern pattern;
        PathPattern.MatchResult matchResult;

        // Test one pattern only pattern
        pattern = PathPattern.newBuilder().one().build();

        matchResult = pattern.match( Path.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "test.test2" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        // Test one pattern after a specific pattern
        pattern = PathPattern.newBuilder().attr( "root" ).one().build();

        matchResult = pattern.match( Path.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root.test2.test3" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );

        // Test one pattern after a any pattern
        pattern = PathPattern.newBuilder().any().one().build();

        matchResult = pattern.match( Path.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.root2.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "root.root2" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 1 ) );

        // Test one pattern after a any pattern
        pattern = PathPattern.newBuilder().one().any().build();

        matchResult = pattern.match( Path.of( "test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 1 ) );

        // Test one pattern before a any pattern and then a specific segment.
        pattern = PathPattern.newBuilder().one().any().attr( "test" ).build();

        matchResult = pattern.match( Path.of( "root.test" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "root" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.test2" ) );
        Assert.assertFalse( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertNull( matchResult.getPart( 0 ) );
        Assert.assertNull( matchResult.getPart( 1 ) );
    }

    @Test
    public void testAnyMatch() throws Exception {
        PathPattern pattern;
        PathPattern.MatchResult matchResult;

        // Test any stuck between two static pattern
        pattern = PathPattern.newBuilder().attr( "root" ).any().attr( "tail" ).build();

        matchResult = pattern.match( Path.of( "root.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root.test.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root.test.test2.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test.test2" ), matchResult.getPart( 0 ) );

        // Test any stuck between two static pattern
        pattern = PathPattern.newBuilder().attr( "root" ).any().any().attr( "tail" ).build();

        matchResult = pattern.match( Path.of( "root.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.test.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 1 ) );

        matchResult = pattern.match( Path.of( "root.test.test2.tail" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 2, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test.test2" ), matchResult.getPart( 0 ) );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 1 ) );

        // Test tail any pattern
        pattern = PathPattern.newBuilder().attr( "root" ).any().build();

        matchResult = pattern.match( Path.of( "root" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root.test1" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test1" ), matchResult.getPart( 0 ) );

        matchResult = pattern.match( Path.of( "root.test1.test2" ) );
        Assert.assertTrue( matchResult.matched() );
        Assert.assertEquals( 1, matchResult.partCount() );
        Assert.assertEquals( Path.of( "test1.test2" ), matchResult.getPart( 0 ) );

    }

    @Test
    public void testOfString() throws Exception {
        Assert.assertEquals( "", PathPattern.of( null ).toString() );
        Assert.assertEquals( "", PathPattern.of( "" ).toString() );
        Assert.assertEquals( "", PathPattern.of( " " ).toString() );
        Assert.assertEquals( "test", PathPattern.of( "test" ).toString() );
        Assert.assertEquals( "test", PathPattern.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", PathPattern.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", PathPattern.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", PathPattern.of( "'test.'" ).toString() );
        Assert.assertEquals( "'test.'.''.'''123'.'123'''", PathPattern.of( "'test.'.''.'''123'.'123'''" ).toString() );
        Assert.assertEquals( "test.*", PathPattern.of( "test.*" ).toString() );
        Assert.assertEquals( "test.**", PathPattern.of( "test.**" ).toString() );
        Assert.assertEquals( "test.*", PathPattern.of( " test.* " ).toString() );
        Assert.assertEquals( "test.**", PathPattern.of( " test.** " ).toString() );
        Assert.assertEquals( "test.*.test2", PathPattern.of( "test.*.test2" ).toString() );
        Assert.assertEquals( "test.**.test2", PathPattern.of( "test.**.test2" ).toString() );
    }

    @Test( expected = ParseException.class )
    public void testOfStringFail1() throws Exception {
        PathPattern.of( "test.* t" );
    }

    @Test( expected = ParseException.class )
    public void testOfStringFail2() throws Exception {
        PathPattern.of( "test.*t" );
    }

    @Test( expected = ParseException.class )
    public void testOfStringFail3() throws Exception {
        PathPattern.of( "test.'test" );
    }


    @Test
    public void testSpecs() throws Exception {
        PathPattern.Specs specs;

        specs = PathPattern.of( "" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.EXACT_MATCH );
        Assert.assertEquals( specs.scope(), Path.of( "" ) );

        specs = PathPattern.of( "*" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "" ) );

        specs = PathPattern.of( "**" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "" ) );

        specs = PathPattern.of( "test" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.EXACT_MATCH );
        Assert.assertEquals( specs.scope(), Path.of( "test" ) );

        specs = PathPattern.of( "test.*" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "test" ) );

        specs = PathPattern.of( "test.**" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "test" ) );

        specs = PathPattern.of( "test.test2" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.EXACT_MATCH );
        Assert.assertEquals( specs.scope(), Path.of( "test.test2" ) );

        specs = PathPattern.of( "test.test2.*" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "test.test2" ) );

        specs = PathPattern.of( "test.test2.**" ).getSpecs();
        Assert.assertEquals( specs.getType(), PathPattern.Type.PARTIAL );
        Assert.assertEquals( specs.scope(), Path.of( "test.test2" ) );

    }

}