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
import org.dbrain.data.text.ReaderCursor;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test path.
 */
public class Path_Test {

    @Test
    public void testEmpty() throws Exception {
        Assert.assertEquals( 0, Path.empty().size() );
        Assert.assertEquals( "", Path.empty().toString() );
    }

    @Test
    public void testEquals() throws Exception {
        Assert.assertTrue( Path.of( "" ).equals( Path.of( "" ) ) );
        Assert.assertTrue( Path.of( "" ).equals( Path.empty() ) );
        Assert.assertTrue( Path.of( "test" ).equals( Path.of( "test" ) ) );
        Assert.assertTrue( Path.of( "test.test2" ).equals( Path.of( "test.test2" ) ) );
        Assert.assertFalse( Path.of( "" ).equals( Path.of( "test" ) ) );
        Assert.assertFalse( Path.of( "test2" ).equals( Path.of( "test" ) ) );
        Assert.assertFalse( Path.of( "test" ).equals( Path.of( "" ) ) );
        Assert.assertFalse( Path.of( "test" ).equals( Path.of( "test.test2" ) ) );
        Assert.assertFalse( Path.of( "test.test2" ).equals( Path.of( "test" ) ) );

        Assert.assertEquals( Path.of( "" ).hashCode(), Path.empty().hashCode() );
        Assert.assertEquals( Path.of( "" ).hashCode(), Path.of( "" ).hashCode() );
        Assert.assertEquals( Path.of( "test" ).hashCode(), Path.of( "test" ).hashCode() );
        Assert.assertEquals( Path.of( "test.test2" ).hashCode(), Path.of( "test.test2" ).hashCode() );
    }


    @Test
    public void testOfReader() throws Exception {
        Assert.assertTrue( Path.of( new ReaderCursor( "" ) ).equals( Path.of( "" ) ) );
        Assert.assertTrue( Path.of( new ReaderCursor( "   " ) ).equals( Path.of( "" ) ) );
        Assert.assertTrue( Path.of( new ReaderCursor( "test.test2" ) ).equals( Path.of( "test.test2" ) ) );
        Assert.assertTrue( Path.of( new ReaderCursor( " test.test2 " ) ).equals( Path.of( "test.test2" ) ) );
    }

    /**
     * Test the parsing method of and toString at the same time. (Which is probably a bad thing).
     */
    @Test
    public void testOfString() throws Exception {
        Assert.assertEquals( "", Path.of( "" ).toString() );
        Assert.assertEquals( "", Path.of( " " ).toString() );
        Assert.assertEquals( "[0]", Path.of( "[0]" ).toString() );
        Assert.assertEquals( "[0][1]", Path.of( "[0][ 1]" ).toString() );
        Assert.assertEquals( "[0][1][2][3]", Path.of( "[0][ 1][2 ][ 3 ]" ).toString() );
        Assert.assertEquals( "[0].toto", Path.of( "[0].toto" ).toString() );
        Assert.assertEquals( "test[0]", Path.of( "'test'[0]" ).toString() );
        Assert.assertEquals( "test", Path.of( "test" ).toString() );
        Assert.assertEquals( "test", Path.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", Path.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", Path.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", Path.of( "'test.'" ).toString() );
        Assert.assertEquals( "'test.'.''.'''123'.'123'''", Path.of( "'test.'.''.'''123'.'123'''" ).toString() );
    }

    /**
     * Test the ofSegment factory method
     */
    @Test
    public void testOfSegment() throws Exception {
        Assert.assertEquals( "", Path.ofAttr( null ).toString() );
        Assert.assertEquals( "''", Path.ofAttr( "" ).toString() );
        Assert.assertEquals( "test", Path.ofAttr( "test" ).toString() );
        Assert.assertEquals( "'test.toto'", Path.ofAttr( "test.toto" ).toString() );
    }

    /**
     * Test builder scenarios.
     */
    @Test
    public void testBuilder() throws Exception {
        Assert.assertEquals( Path.of( "" ), Path.newBuilder().build() );
        Assert.assertEquals( Path.of( "test" ), Path.newBuilder().attr( "test" ).build() );
        Assert.assertEquals( Path.of( "test" ), Path.fromAttr( "test" ).build() );
        Assert.assertEquals( Path.of( "test.test2" ), Path.newBuilder().attr( "test" ).attr( "test2" ).build() );
        Assert.assertEquals( Path.of( "test.test2" ), Path.fromAttr( "test" ).attr( "test2" ).build() );
        Assert.assertEquals( Path.of( "test.test2.test3" ),
                             Path.from( Path.of( "test.test2" ) ).attr( "test3" ).build() );
        Assert.assertEquals( Path.of( "test" ), Path.from( null ).attr( "test" ).build() );
        Assert.assertEquals( Path.of( "test.test2.test3" ),
                             Path.newBuilder().attr( "test" ).append( Path.of( "test2.test3" ) ).build() );
        Assert.assertEquals( Path.of( "test" ), Path.newBuilder().attr( "test" ).append( (Path)null ).build() );
    }

    /**
     * Test invalid encoding.
     */
    @Test
    public void testOfString_invalid() throws Exception {
        String[] invalidFqn = { ".", "*", "[", "]", "*", ",", "test. toto", "test[0]toto", "test[-1].toto" };

        for ( String s : invalidFqn ) {
            try {
                Path.of( s );
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
        Assert.assertEquals( 1, Path.of( "''" ).size() );
        Assert.assertEquals( 2, Path.of( "'test*'.''" ).size() );
        Assert.assertEquals( 3, Path.of( "'test'''.'*'.'**'" ).size() );
        Assert.assertEquals( 4, Path.of( "a1.a2.a3.a4" ).size() );

        Assert.assertEquals( 1, Path.of( "[0]" ).size() );
        Assert.assertEquals( 2, Path.of( "test[0]" ).size() );
        Assert.assertEquals( 3, Path.of( "test[1][2]" ).size() );
        Assert.assertEquals( 4, Path.of( "test[1][2].toto" ).size() );
        Assert.assertEquals( 5, Path.of( "test[ 1][2 ].toto[ 3 ]" ).size() );
    }

    /**
     * Test the segment method.
     */
    @Test
    public void testAttr() throws Exception {
        Assert.assertEquals( "test", Path.of( "test" ).attr( 0 ) );
        Assert.assertEquals( "", Path.of( "''" ).attr( 0 ) );
        Assert.assertEquals( "*", Path.of( "test.'*'" ).attr( 1 ) );
        Assert.assertEquals( "", Path.of( "test.''" ).attr( 1 ) );
    }

    /**
     * Test the segment method.
     */
    @Test
    public void testIndex() throws Exception {
        Assert.assertEquals( 1L, Path.of( "test[1]" ).index( 1 ) );
        Assert.assertEquals( 2L, Path.of( "[2]" ).index( 0 ) );
        Assert.assertEquals( 3L, Path.of( "test.'*'[3]" ).index( 2 ) );
    }

    @Test
    public void testHead() throws Exception {
        Assert.assertEquals( Path.of( "test.toto.tutu" ), Path.of("test.toto.tutu" ).head( 3 ) );
        Assert.assertEquals( Path.of( "test.toto" ), Path.of("test.toto.tutu" ).head( 2 ) );
        Assert.assertEquals( Path.of( "test" ), Path.of("test.toto.tutu" ).head( 1 ) );
        Assert.assertEquals( Path.empty(), Path.of("test.toto.tutu" ).head( 0 ) );
    }

    @Test
    public void testTail() throws Exception {
        Assert.assertEquals( Path.empty(), Path.of("test.toto.tutu" ).tail( 0 ) );
        Assert.assertEquals( Path.of( "tutu" ), Path.of("test.toto.tutu" ).tail( 1 ) );
        Assert.assertEquals( Path.of( "toto.tutu" ), Path.of("test.toto.tutu" ).tail( 2 ) );
        Assert.assertEquals( Path.of( "test.toto.tutu" ), Path.of("test.toto.tutu" ).tail( 3 ) );
    }


    @Test
    public void testTailFrom() throws Exception {
        Assert.assertEquals( Path.empty(), Path.of("test.toto.tutu" ).tailFrom( 3 ) );
        Assert.assertEquals( Path.of( "tutu" ), Path.of("test.toto.tutu" ).tailFrom( 2 ) );
        Assert.assertEquals( Path.of( "toto.tutu" ), Path.of("test.toto.tutu" ).tailFrom( 1 ) );
        Assert.assertEquals( Path.of( "test.toto.tutu" ), Path.of("test.toto.tutu" ).tailFrom( 0 ) );
    }


    @Test( expected = IndexOutOfBoundsException.class )
    public void testOutOfBoundSegment1() throws Exception {
        Path.of( "test" ).attr( 1 );
    }

    @Test( expected = IndexOutOfBoundsException.class )
    public void testOutOfBoundSegment2() throws Exception {
        Path.of( "" ).attr( 0 );
    }


    @Test
    public void testStartsWith() throws Exception {

        // Those are true
        Assert.assertTrue( Path.of( "test.a123" ).startsWith( Path.of( "test" ) ) );
        Assert.assertTrue( Path.of( "test.a123" ).startsWith( Path.of( "test.a123" ) ) );
        Assert.assertTrue( Path.of( "test.a123.a456" ).startsWith( Path.of( "test" ) ) );
        Assert.assertTrue( Path.of( "test.a123.a456" ).startsWith( Path.of( "test.a123" ) ) );
        Assert.assertTrue( Path.of( "test.a123" ).startsWith( Path.of( "" ) ) );
        Assert.assertTrue( Path.of( "test.a123" ).startsWith( null ) );
        Assert.assertTrue( Path.of( "" ).startsWith( Path.of( "" ) ) );

        // Those are false
        Assert.assertFalse( Path.of( "test.a123" ).startsWith( Path.of( "other" ) ) );
        Assert.assertFalse( Path.of( "test" ).startsWith( Path.of( "test.a123" ) ) );
        Assert.assertFalse( Path.of( "test.a123" ).startsWith( Path.of( "test.a123.a456" ) ) );
        Assert.assertFalse( Path.of( "" ).startsWith( Path.of( "test" ) ) );

    }


}
